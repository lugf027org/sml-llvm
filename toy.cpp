/****************************
*解释器构造实验
*Author Lu
*VS 2019
*LLVM 11月https://github.com/llvm/llvm-project的master版本，可能是9.X版本？
*将本代码复制到 Kaleidoscope-ch7的toy.cpp后运行即可。
*****************************/

#include "../include/KaleidoscopeJIT.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

using namespace llvm;
using namespace llvm::orc;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//
enum Token {
  // end
  tok_eof = -1,

  // commands
  tok_fun = -2,
  tok_extern = -3,

  // primary
  tok_identifier = -4,
  tok_double_num = -5,
  tok_int_num = -6,

  // control
  tok_if = -7,
  tok_then = -8,
  tok_else = -9,

  tok_while = -10,
  tok_do = -11,

  // operators
  tok_binary = -12,
  tok_unary = -13,

  // val definition
  tok_let = -14,
  tok_val = -15,
  tok_end = -16,
  tok_in = -17,

  tok_real = -18,
  tok_int = -19
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_double_num or tok_int_num

///
bool isSymbol(int ch) {
  if (ch == '!' || ch == '%' || ch == '&' ||
      ch == '$' || ch == '#' ||          /* ch == '+'
|| ch == '-' || ch == '*' ||*/ ch == '/' /*|| ch == ':' || ch == '<' || ch ==
                                            '='*/
                                         /*|| ch == '>'*/
      || ch == '?' || ch == '@' || ch == '\\' || ch == '~' || ch == '`' ||
      ch == '^' || ch == '|')
    return true;
  else
    return false;
}

/// gettok - Return the next token from standard input.
static int gettok() {
  static int LastChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = getchar();

  if (isalpha(LastChar)) { // identifier: [a-zA-Z][_'a-zA-Z0-9]*
    IdentifierStr = LastChar;
    while (isalnum((LastChar = getchar())) || LastChar == '\'' ||
           LastChar == '_')
      IdentifierStr += LastChar;

    if (IdentifierStr == "fun")
      return tok_fun;
    if (IdentifierStr == "extern")
      return tok_extern;
    if (IdentifierStr == "if")
      return tok_if;
    if (IdentifierStr == "then")
      return tok_then;
    if (IdentifierStr == "else")
      return tok_else;
    if (IdentifierStr == "while")
      return tok_while;
    if (IdentifierStr == "do")
      return tok_do;
    if (IdentifierStr == "binary")
      return tok_binary;
    if (IdentifierStr == "unary")
      return tok_unary;
    if (IdentifierStr == "val")
      return tok_val;
    if (IdentifierStr == "let")
      return tok_let;
    if (IdentifierStr == "end")
      return tok_end;
    if (IdentifierStr == "in")
      return tok_in;
    if (IdentifierStr == "real")
      return tok_real;
    if (IdentifierStr == "int")
      return tok_int;
    return tok_identifier;
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
    bool isDouble = false;
    std::string NumStr;
    do {
      if (LastChar == '.') {
        isDouble = true;
      }
      NumStr += LastChar;
      LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), nullptr);
    if (isDouble) {
      return tok_double_num;
    }
    return tok_int_num;
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF)
    return tok_eof;

  if (isSymbol(LastChar)) { // Identifier of Symbol
    IdentifierStr = LastChar;
    while (isSymbol(LastChar = getchar()))
      IdentifierStr += LastChar;
    return tok_identifier;
  }

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() = default;

  virtual Value *codegen() = 0;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  double Val;
  std::string type;

public:
  NumberExprAST(double Val, std::string type) : Val(Val), type(type) {}

  Value *codegen() override;
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string Name;

public:
  VariableExprAST(const std::string &Name) : Name(Name) {}

  Value *codegen() override;
  const std::string &getName() const { return Name; }
};

/// UnaryExprAST - Expression class for a unary operator.
class UnaryExprAST : public ExprAST {
  char Opcode;
  std::unique_ptr<ExprAST> Operand;

public:
  UnaryExprAST(char Opcode, std::unique_ptr<ExprAST> Operand)
      : Opcode(Opcode), Operand(std::move(Operand)) {}

  Value *codegen() override;
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;

public:
  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                std::unique_ptr<ExprAST> RHS)
      : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

  Value *codegen() override;
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  CallExprAST(const std::string &Callee,
              std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee), Args(std::move(Args)) {}

  Value *codegen() override;
};

/// IfExprAST - Expression class for if/then/else.
class IfExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Cond, Then, Else;

public:
  IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
            std::unique_ptr<ExprAST> Else)
      : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

  Value *codegen() override;
};

/// WhileExprAST - Expression class for while/do/end.
class WhileExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Cond, Body, ValueToRet;

public:
  WhileExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Body,
               std::unique_ptr<ExprAST> ValueToRet)
      : Cond(std::move(Cond)), Body(std::move(Body)),
        ValueToRet(std::move(ValueToRet)) {}

  Value *codegen() override;
};

/// LetExprAST - Expression class for var/in
class LetExprAST : public ExprAST {
  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;
  std::unique_ptr<ExprAST> Body;

public:
  LetExprAST(
      std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames,
      std::unique_ptr<ExprAST> Body)
      : VarNames(std::move(VarNames)), Body(std::move(Body)) {}

  Value *codegen() override;
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes), as well as if it is an operator.
class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;
  std::string NameType;
  std::string ArgsType;

  bool IsOperator;
  unsigned Precedence; // Precedence if a binary op.

public:
  PrototypeAST(const std::string &Name, std::vector<std::string> Args,
               std::string NameType, std::string ArgsType,
               bool IsOperator = false, unsigned Prec = 0)
      : Name(Name), Args(std::move(Args)), NameType(NameType),
        ArgsType(ArgsType), IsOperator(IsOperator), Precedence(Prec) {}

  Function *codegen();
  const std::string &getName() const { return Name; }

  bool isUnaryOp() const { return IsOperator && Args.size() == 1; }
  bool isBinaryOp() const { return IsOperator && Args.size() == 2; }

  char getOperatorName() const {
    assert(isUnaryOp() || isBinaryOp());
    return Name[Name.size() - 1];
  }

  unsigned getBinaryPrecedence() const { return Precedence; }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;
  std::string Name;

public:
  FunctionAST(std::string Name, std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body)
      : Name(Name), Proto(std::move(Proto)), Body(std::move(Body)) {}

  const std::string &getName() const { return Name; }
  Function *codegen(Type **retype);
};

class ValAST : public ExprAST {
  std::string Name;
  std::unique_ptr<ExprAST> val;

public:
  ValAST(const std::string &Name, std::unique_ptr<ExprAST> val)
      : Name(Name), val(std::move(val)) {}

  // GlobalVariable *codegen();
  Value *codegen() override;
  const std::string &getName() const { return Name; }
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;
static bool isDefineExternNow = false;
static int getNextToken() { return CurTok = gettok(); }

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
  int TokPrec;
  if (!isascii(CurTok))
    return -1;

  TokPrec = BinopPrecedence[CurTok];
  if (TokPrec <= 0)
    return -1;
  return TokPrec;
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str) {
  fprintf(stderr, "Read \033[40;31mError: %s\033[0m\nsml->", Str); // With color
  return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

std::unique_ptr<ValAST> LogErrorVal(const char *Str) {
  LogError(Str);
  return nullptr;
}

Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto Result = std::make_unique<NumberExprAST>(NumVal, "real");
  if (CurTok == tok_int_num) {
    Result = std::make_unique<NumberExprAST>(NumVal, "int");
  }
  getNextToken(); // consume the number
  return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
  getNextToken(); // eat (.
  auto V = ParseExpression();
  if (!V)
    return nullptr;

  if (CurTok != ')')
    return LogError("Expected ')'");
  getNextToken(); // eat ).
  return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken(); // eat identifier.

  if (CurTok != '(') // Simple variable ref.
    return std::make_unique<VariableExprAST>(IdName);

  // Call.
  getNextToken(); // eat (
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != ')') {
    while (true) {
      if (auto Arg = ParseExpression())
        Args.push_back(std::move(Arg));
      else
        return nullptr;

      if (CurTok == ')')
        break;

      if (CurTok != ',')
        return LogError("Expected ')' or ',' in argument list");
      getNextToken();
    }
  }

  getNextToken(); // Eat the ')'.

  return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// ifexpr ::= 'if' expression 'then' expression 'else' expression
static std::unique_ptr<ExprAST> ParseIfExpr() {
  getNextToken(); // eat the if.

  // condition.
  auto Cond = ParseExpression();
  if (!Cond)
    return nullptr;

  if (CurTok != tok_then)
    return LogError("Expected then keyword");
  getNextToken(); // eat the then

  auto Then = ParseExpression();
  if (!Then)
    return nullptr;

  if (CurTok != tok_else)
    return LogError("Expected else keyword");

  getNextToken();

  auto Else = ParseExpression();
  if (!Else)
    return nullptr;

  return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
                                     std::move(Else));
}

/// whileexpr	::= 'while' exprssion 'do' expression ';' expression
static std::unique_ptr<ExprAST> ParseWhileExpr() {
  getNextToken(); // eat while

  auto Cond = ParseExpression();
  if (!Cond)
    return nullptr;

  if (CurTok != tok_do)
    return LogError("expected do keyword");
  getNextToken(); // eat do

  auto Body = ParseExpression();
  if (!Body)
    return nullptr;

  if (CurTok != ';')
    return LogError("expected ';'");
  getNextToken(); // eat ';'

  auto ValuetoReturn = ParseExpression();
  if (!Body)
    return nullptr;

  return std::make_unique<WhileExprAST>(std::move(Cond), std::move(Body),
                                        std::move(ValuetoReturn));
}

///  letexpr ::= 'let' 'val' identifier : type  ('=' expression)?
///                   (',' 'val' identifier : type ('=' expression)?)* 'in'
///                   expression
///                  'end'
static std::unique_ptr<ExprAST> ParseLetExpr() {
  getNextToken(); // eat the let.

  if (CurTok != tok_val)
    return LogError("expected var keyword after let");
  getNextToken(); // Eat the val

  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;
  std::vector<std::pair<std::string, std::string>> VarTypes;

  // At least one variable name is required.
  if (CurTok != tok_identifier)
    return LogError("expected identifier after let");

  while (true) {
    std::string Name = IdentifierStr;
    getNextToken(); // eat identifier.

    if (CurTok != ':')
      return LogErrorVal("Expected ':'");
    getNextToken();

    if (CurTok == tok_real)
      VarTypes.push_back(std::make_pair(Name, "real"));
    else if (CurTok == tok_int)
      VarTypes.push_back(std::make_pair(Name, "int"));
    else
      return LogErrorVal("Expected Args Type");
    getNextToken();

    // Read the optional initializer.
    std::unique_ptr<ExprAST> Init = nullptr;
    if (CurTok == '=') {
      getNextToken(); // eat '='.

      Init = ParseExpression();
      if (!Init)
        return nullptr;
    }

    VarNames.push_back(std::make_pair(Name, std::move(Init)));

    // End of var list, exit loop.
    if (CurTok != ';')
      break;
    getNextToken(); // eat the ','.

    if (CurTok != tok_val)
      return LogError("expected var keyword");
    getNextToken(); // Eat the val

    if (CurTok != tok_identifier)
      return LogError("expected identifier");
  }

  // At this point, we have to have 'in'.
  if (CurTok != tok_in)
    return LogError("expected in keyword");
  getNextToken(); // eat 'in'.

  auto Body = ParseExpression();
  if (!Body)
    return nullptr;

  if (CurTok != tok_end)
    return LogError("expected 'end' keyword to end 'let'");
  getNextToken();

  return std::make_unique<LetExprAST>(std::move(VarNames), std::move(Body));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
///   ::= ifexpr
///   ::= forexpr
///   ::= varexpr
///   ::= whileexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
  switch (CurTok) {
  default:
    return LogError("unknown token when expecting an expression");
  case tok_identifier:
    return ParseIdentifierExpr();
  case tok_double_num:
    return ParseNumberExpr();
  case tok_int_num:
    return ParseNumberExpr();
  case '(':
    return ParseParenExpr();
  case tok_if:
    return ParseIfExpr();
  case tok_let:
    return ParseLetExpr();
  case tok_while:
    return ParseWhileExpr();
  }
}

/// unary
///   ::= primary
///   ::= '!' unary
static std::unique_ptr<ExprAST> ParseUnary() {
  // If the current token is not an operator, it must be a primary expr.
  if (!isascii(CurTok) || CurTok == '(' || CurTok == ',')
    return ParsePrimary();

  // If this is a unary operator, read it.
  int Opc = CurTok;
  getNextToken();
  if (auto Operand = ParseUnary())
    return std::make_unique<UnaryExprAST>(Opc, std::move(Operand));
  return nullptr;
}

/// binoprhs
///   ::= ('+' unary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) {
  // If this is a binop, find its precedence.
  while (true) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec)
      return LHS;

    // Okay, we know this is a binop.
    int BinOp = CurTok;
    getNextToken(); // eat binop

    // Parse the unary expression after the binary operator.
    auto RHS = ParseUnary();
    if (!RHS)
      return nullptr;

    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int NextPrec = GetTokPrecedence();
    if (TokPrec < NextPrec) {
      RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
      if (!RHS)
        return nullptr;
    }

    // Merge LHS/RHS.
    LHS =
        std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

/// expression
///   ::= unary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParseUnary();
  if (!LHS)
    return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

/// prototype
///   ::= id '(' (id ':' type ',')* ')' ':' type '='
///   ::= binary LETTER number? (id, id)
///   ::= unary LETTER (id)
static std::unique_ptr<PrototypeAST> ParsePrototype() {
  std::string FnName;
  std::string NameType;
  std::string ArgsType;

  unsigned Kind = 0; // 0 = identifier, 1 = unary, 2 = binary.
  unsigned BinaryPrecedence = 30;

  switch (CurTok) {
  default:
    return LogErrorP("Expected function name in prototype");
  case tok_identifier:
    FnName = IdentifierStr;
    Kind = 0;
    getNextToken();
    break;
  case tok_unary:
    getNextToken();
    if (!isascii(CurTok))
      return LogErrorP("Expected unary operator");
    FnName = "unary";
    FnName += (char)CurTok;
    Kind = 1;
    getNextToken();
    break;
  case tok_binary:
    getNextToken();
    if (!isascii(CurTok))
      return LogErrorP("Expected binary operator");
    FnName = "binary";
    FnName += (char)CurTok;
    Kind = 2;
    getNextToken();

    // Read the precedence if present.
    if (CurTok == tok_double_num || CurTok == tok_int_num) {
      if (NumVal < 1 || NumVal > 100)
        return LogErrorP("Invalid precedence: must be 1..100");
      BinaryPrecedence = (unsigned)NumVal;
      getNextToken();
    }
    break;
  }

  // id '(' (id ':' type ',')* ')' ':' type '='
  if (CurTok != '(')
    return LogErrorP("Expected '(' in prototype");

  std::vector<std::string> ArgNames;
  while (getNextToken() == tok_identifier) {
    ArgNames.push_back(IdentifierStr);

    if (getNextToken() != ':')
      return LogErrorP("Expected ':' in prototype");

    if (getNextToken() == tok_real)
      ArgsType = "real";
    else if (CurTok == tok_int)
      ArgsType = "int";
    else
      return LogErrorP("Expected Type of Args in prototype");

    if (getNextToken() == ')')
      break;
    if (CurTok != ',')
      return LogErrorP("Expected ',' in prototype");
  }

  if (CurTok != ')')
    return LogErrorP("Expected ')' in prototype");

  if (getNextToken() != ':')
    return LogErrorP("Expected ':' in prototype");

  if (getNextToken() == tok_real)
    NameType = "real";
  else if (CurTok == tok_int)
    NameType = "int";
  else
    return LogErrorP("Expected Type To Return in prototype");

  getNextToken(); // eat type
  if (isDefineExternNow == true) {
    isDefineExternNow = false;
    if (Kind && ArgNames.size() != Kind)
      return LogErrorP("Invalid number of operands for operator");
    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames), NameType,
                                          ArgsType, Kind != 0,
                                          BinaryPrecedence);
  }
  if (CurTok != '=')
    return LogErrorP("Expected '=' in prototype");

  // success.
  getNextToken(); // eat '='.

  // Verify right number of names for operator.
  if (Kind && ArgNames.size() != Kind)
    return LogErrorP("Invalid number of operands for operator");

  return std::make_unique<PrototypeAST>(FnName, ArgNames, NameType, ArgsType,
                                        Kind != 0, BinaryPrecedence);
}

/// definition ::= 'fun' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition() {
  getNextToken(); // eat fun.
  auto Proto = ParsePrototype();
  if (!Proto)
    return nullptr;

  std::string Name = Proto->getName();
  if (auto E = ParseExpression())
    return std::make_unique<FunctionAST>(Name, std::move(Proto), std::move(E));
  return nullptr;
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
  if (auto E = ParseExpression()) {

    // Make an anonymous proto.
    auto Proto = std::make_unique<PrototypeAST>(
        "__anon_expr", std::vector<std::string>(), "real", "real");
    std::string Name = Proto->getName();
    return std::make_unique<FunctionAST>(Name, std::move(Proto), std::move(E));
  }
  return nullptr;
}

/// external ::= 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern() {
  getNextToken(); // eat extern.
  isDefineExternNow = true;
  return ParsePrototype();
}

/// valExpr ::= 'val' identifier ':' type = Expression
static std::unique_ptr<ValAST> ParseValExpr() {
  if (getNextToken() != tok_identifier) {
    return nullptr;
  }
  std::string valName = IdentifierStr;

  if (getNextToken() != ':') {
    return LogErrorVal("expect ':'");
  }

  std::string valType; // In fact, the valType is not used for the Value* has
                       // record the Type already
  if (getNextToken() == tok_real)
    valType = "real";
  else if (CurTok == tok_int)
    valType = "int";
  else
    return LogErrorVal("expect type");

  if (getNextToken() != '=')
    return LogErrorVal("expect '='");

  getNextToken(); // eat =

  if (auto E = ParseExpression())
    return std::make_unique<ValAST>(valName, std::move(E));
  return nullptr;
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
static std::map<std::string, AllocaInst *> NamedValues;
/// NameValues_Global - This store the global values.
static std::map<std::string, Value *> NamedValues_Global;
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;

Function *getFunction(std::string Name) {
  // First, see if the function has already been added to the current module.
  if (auto *F = TheModule->getFunction(Name))
    return F;

  // If not, check whether we can codegen the declaration from some existing
  // prototype.
  auto FI = FunctionProtos.find(Name);
  if (FI != FunctionProtos.end())
    return FI->second->codegen();

  // If no existing prototype exists, return null.
  return nullptr;
}

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function.  This is used for mutable variables etc.
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
                                          const std::string &VarName,
                                          std::string type) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                   TheFunction->getEntryBlock().begin());
  if (type == "real")
    return TmpB.CreateAlloca(Type::getDoubleTy(TheContext), nullptr, VarName);
  else
    return TmpB.CreateAlloca(Type::getInt32Ty(TheContext), nullptr, VarName);
}

Value *NumberExprAST::codegen() {
  if (type == "real") {
    return ConstantFP::get(TheContext, APFloat(Val));
  } else {
    return ConstantInt::get(Type::getInt32Ty(TheContext), (int32_t)Val, true);
  }
}

Value *VariableExprAST::codegen() {
  // Look this variable up in the function.
  Value *V = NamedValues[Name];
  if (!V) {
    V = NamedValues_Global[Name];
    if (!V)
      return LogErrorV("Unknown variable name");
    else
      return V;
  }

  // Load the value.
  return Builder.CreateLoad(V, Name.c_str());
}

Value *UnaryExprAST::codegen() {
  Value *OperandV = Operand->codegen();
  if (!OperandV)
    return nullptr;

  Function *F = getFunction(std::string("unary") + Opcode);
  if (!F)
    return LogErrorV("Unknown unary operator");

  return Builder.CreateCall(F, OperandV, "unop");
}

Value *BinaryExprAST::codegen() {
  // Special case '=' because we don't want to emit the LHS as an expression.
  if (Op == '=') {
    // Assignment requires the LHS to be an identifier.
    // This assume we're building without RTTI because LLVM builds that way by
    // default.  If you build LLVM with RTTI this can be changed to a
    // dynamic_cast for automatic error checking.
    VariableExprAST *LHSE = static_cast<VariableExprAST *>(LHS.get());
    if (!LHSE)
      return LogErrorV("destination of '=' must be a variable");
    // Codegen the RHS.
    Value *Val = RHS->codegen();
    if (!Val)
      return nullptr;

    // Look up the name.
    Value *Variable = NamedValues[LHSE->getName()];
    if (!Variable) {
      Variable = NamedValues_Global[LHSE->getName()];
      if (!Variable)
        return LogErrorV("Unknown variable name");
      else {
        NamedValues_Global[LHSE->getName()] = Val;
        return Val;
      }
    }
    Builder.CreateStore(Val, Variable);
    return Val;
  }

  Value *L = LHS->codegen();
  Value *R = RHS->codegen();
  if (!L || !R)
    return nullptr;

  switch (Op) {
  case '+':
    if ((L->getType() == Type::getDoubleTy(TheContext)) ||
        (R->getType() == Type::getDoubleTy(TheContext))) {
      return Builder.CreateFAdd(
          Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext)),
          Builder.CreateUIToFP(R, Type::getDoubleTy(TheContext)), "addtmp");
    } else {
      return Builder.CreateAdd(L, R, "addtmp");
    }
  case '-':
    if ((L->getType() == Type::getDoubleTy(TheContext)) ||
        (R->getType() == Type::getDoubleTy(TheContext))) {
      return Builder.CreateFSub(
          Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext)),
          Builder.CreateUIToFP(R, Type::getDoubleTy(TheContext)), "subtmp");
    } else {
      return Builder.CreateSub(L, R, "subtmp");
    }
  case '*':
    if ((L->getType() == Type::getDoubleTy(TheContext)) ||
        (R->getType() == Type::getDoubleTy(TheContext))) {
      return Builder.CreateFMul(
          Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext)),
          Builder.CreateUIToFP(R, Type::getDoubleTy(TheContext)), "multmp");
    } else {
      return Builder.CreateMul(L, R, "multmp");
    }
  case '<':
    if (L->getType() == Type::getDoubleTy(TheContext)) {
      L = Builder.CreateFCmpULT(L, R, "cmptmp");
      // Convert bool 0/1 to double 0.0 or 1.0
      // return Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext),
      // "booltmp");
      return Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext), "booltmp");
    } else {
      L = Builder.CreateICmpULT(L, R, "cmptmp");
      // If int cmp, just return the value.
      return L;
    }
  case '>':
    if (L->getType() == Type::getDoubleTy(TheContext)) {
      L = Builder.CreateFCmpULT(R, L, "cmptmp");
      // Convert bool 0/1 to double 0.0 or 1.0
      // return Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext),
      // "booltmp");
      return Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext), "booltmp");
    } else {
      L = Builder.CreateICmpULT(R, L, "cmptmp");
      // If int cmp, just return the value.
      return L;
    }
  default:
    break;
  }

  // If it wasn't a builtin binary operator, it must be a user defined one. Emit
  // a call to it.
  Function *F = getFunction(std::string("binary") + Op);
  assert(F && "binary operator not found!");

  Value *Ops[] = {L, R};
  return Builder.CreateCall(F, Ops, "binop");
}

Value *CallExprAST::codegen() {
  // Look up the name in the global module table.
  Function *CalleeF = getFunction(Callee);
  if (!CalleeF)
    return LogErrorV("Unknown function referenced");

  // If argument mismatch error.
  if (CalleeF->arg_size() != Args.size())
    return LogErrorV("Incorrect # arguments passed");

  std::vector<Value *> ArgsV;
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back())
      return nullptr;
  }

  return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

Value *IfExprAST::codegen() {
  Value *CondV = Cond->codegen();
  if (!CondV)
    return nullptr;

  // Convert condition to a bool by comparing non-equal to 0.0.
  bool ifConVDouble = false;
  if (CondV->getType() == Type::getDoubleTy(TheContext)) {
    ifConVDouble = true;

    CondV = Builder.CreateFCmpONE(
        CondV, ConstantFP::get(TheContext, APFloat(0.0)), "ifcond");
  }

  else {
    ifConVDouble = false;
    CondV = Builder.CreateICmpNE(
        CondV, ConstantInt::get(Type::getInt32Ty(TheContext), (int32_t)0, true),
        "ifcond");
  }

  Function *TheFunction = Builder.GetInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at the
  // end of the function.
  BasicBlock *ThenBB = BasicBlock::Create(TheContext, "then", TheFunction);
  BasicBlock *ElseBB = BasicBlock::Create(TheContext, "else");
  BasicBlock *MergeBB = BasicBlock::Create(TheContext, "ifcont");

  Builder.CreateCondBr(CondV, ThenBB, ElseBB);

  // Emit then value.
  Builder.SetInsertPoint(ThenBB);

  Value *ThenV = Then->codegen();
  if (!ThenV)
    return nullptr;

  Builder.CreateBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = Builder.GetInsertBlock();

  // Emit else block.
  TheFunction->getBasicBlockList().push_back(ElseBB);
  Builder.SetInsertPoint(ElseBB);

  Value *ElseV = Else->codegen();
  if (!ElseV)
    return nullptr;

  Builder.CreateBr(MergeBB);
  // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
  ElseBB = Builder.GetInsertBlock();

  // Emit merge block.
  TheFunction->getBasicBlockList().push_back(MergeBB);
  Builder.SetInsertPoint(MergeBB);

  PHINode *PN = nullptr;

  if (ifConVDouble)
    PN = Builder.CreatePHI(Type::getDoubleTy(TheContext), 2, "iftmp");
  else
    PN = Builder.CreatePHI(Type::getInt32Ty(TheContext), 2, "iftmp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);

  return PN;
}

Value *WhileExprAST::codegen() {
  Function *TheFunction = Builder.GetInsertBlock()->getParent();

  // Make the new basic block for the loop header, inserting after current
  // block.
  BasicBlock *LoopBB = BasicBlock::Create(TheContext, "loop", TheFunction);

  // Insert an explicit fall through from the current block to the LoopBB.
  Builder.CreateBr(LoopBB);

  // Start insertion in LoopBB.
  Builder.SetInsertPoint(LoopBB);

  // Emit the body of the loop.  This, like any other expr, can change the
  // current BB.  Note that we ignore the value computed by the body, but don't
  // allow an error.
  if (!Body->codegen())
    return nullptr;

  // Compute the end condition.
  Value *EndCond = Cond->codegen();
  if (!EndCond)
    return nullptr;

  // Convert condition to a bool by comparing non-equal to 0.0.
  EndCond = Builder.CreateFCmpONE(
      EndCond, ConstantFP::get(TheContext, APFloat(0.0)), "loopcond");
  // Create the "after loop" block and insert it.
  BasicBlock *AfterBB =
      BasicBlock::Create(TheContext, "afterloop", TheFunction);

  // Insert the conditional branch into the end of LoopEndBB.
  Builder.CreateCondBr(EndCond, LoopBB, AfterBB);

  // Any new code will be inserted in AfterBB.
  Builder.SetInsertPoint(AfterBB);

  Value *ValueReturnLater = ValueToRet->codegen();
  return ValueReturnLater;
}

Value *LetExprAST::codegen() {
  std::vector<AllocaInst *> OldBindings;

  Function *TheFunction = Builder.GetInsertBlock()->getParent();

  // Register all variables and emit their initializer.
  for (unsigned i = 0, e = VarNames.size(); i != e; ++i) {
    const std::string &VarName = VarNames[i].first;
    ExprAST *Init = VarNames[i].second.get();

    // Emit the initializer before adding the variable to scope, this prevents
    // the initializer from referencing the variable itself, and permits stuff
    // like this:
    //  var a = 1 in
    //    var a = a in ...   # refers to outer 'a'.
    Value *InitVal;
    if (Init) {
      InitVal = Init->codegen();
      if (!InitVal)
        return nullptr;
    } else { // If not specified, use 0.0.
      InitVal = ConstantFP::get(TheContext, APFloat(0.0));
    }

    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName, "real");
    Builder.CreateStore(InitVal, Alloca);

    // Remember the old variable binding so that we can restore the binding when
    // we unrecurse.
    OldBindings.push_back(NamedValues[VarName]);

    // Remember this binding.
    NamedValues[VarName] = Alloca;
  }

  // Codegen the body, now that all vars are in scope.
  Value *BodyVal = Body->codegen();
  if (!BodyVal)
    return nullptr;

  // Pop all our variables from scope.
  for (unsigned i = 0, e = VarNames.size(); i != e; ++i)
    NamedValues[VarNames[i].first] = OldBindings[i];

  // Return the body computation.
  return BodyVal;
}

Function *PrototypeAST::codegen() {
  // Make the function type:  double(double,double) etc.
  FunctionType *FT = nullptr;

  if (ArgsType == "real") {
    std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(TheContext));
    if (NameType == "real") {
      FT = FunctionType::get(Type::getDoubleTy(TheContext), Doubles, false);
    } else if (NameType == "int") {
      FT = FunctionType::get(Type::getInt32Ty(TheContext), Doubles, false);
    }
  } else if (ArgsType == "int") {
    std::vector<Type *> Ints(Args.size(), Type::getInt32Ty(TheContext));
    if (NameType == "real") {
      FT = FunctionType::get(Type::getDoubleTy(TheContext), Ints, false);
    } else if (NameType == "int") {
      FT = FunctionType::get(Type::getInt32Ty(TheContext), Ints, false);
    }
  }
  Function *F =
      Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());
  // Set names for all arguments.
  unsigned Idx = 0;
  for (auto &Arg : F->args())
    Arg.setName(Args[Idx++]);

  return F;
}

Function *FunctionAST::codegen(Type **TypeToReturn) {
  // Transfer ownership of the prototype to the FunctionProtos map, but keep a
  // reference to it for use below.
  auto &P = *Proto;
  FunctionProtos[Proto->getName()] = std::move(Proto);
  Function *TheFunction = getFunction(P.getName());
  if (!TheFunction)
    return nullptr;

  // If this is an operator, install it.
  if (P.isBinaryOp())
    BinopPrecedence[P.getOperatorName()] = P.getBinaryPrecedence();

  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
  Builder.SetInsertPoint(BB);

  // Record the function arguments in the NamedValues map.
  NamedValues.clear();
  for (auto &Arg : TheFunction->args()) {
    AllocaInst *Alloca;
    if (TheFunction->getReturnType() == Type::getDoubleTy(TheContext))

      // Create an alloca for this variable.
      Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getName(), "real");

    else if (TheFunction->getReturnType() == Type::getInt32Ty(TheContext))
      Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getName(), "int");

    // Store the initial value into the alloca.
    Builder.CreateStore(&Arg, Alloca);

    // Add arguments to variable symbol table.
    NamedValues[Arg.getName()] = Alloca;
  }

  if (Value *RetVal = Body->codegen()) {
    // Give the back type
    *TypeToReturn = RetVal->getType();

    // Finish off the function.
    Builder.CreateRet(RetVal);

    // Validate the generated code, checking for consistency.
    verifyFunction(*TheFunction);

    // Run the optimizer on the function.
    TheFPM->run(*TheFunction);

    return TheFunction;
  }

  // Error reading body, remove function.
  TheFunction->eraseFromParent();

  if (P.isBinaryOp())
    BinopPrecedence.erase(P.getOperatorName());
  return nullptr;
}

Value *ValAST::codegen() {
  NamedValues_Global[Name] = val->codegen();
  return val->codegen();
}

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static void InitializeModuleAndPassManager() {
  // Open a new module.
  TheModule = std::make_unique<Module>("Standard ML", TheContext);
  TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());

  // Create a new pass manager attached to it.
  TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());

  // Promote allocas to registers.
  TheFPM->add(createPromoteMemoryToRegisterPass());
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  TheFPM->add(createInstructionCombiningPass());
  // Reassociate expressions.
  TheFPM->add(createReassociatePass());
  // Eliminate Common SubExpressions.
  TheFPM->add(createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  TheFPM->add(createCFGSimplificationPass());

  TheFPM->doInitialization();
}

static void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    Type *reType;
    if (auto *FnIR = FnAST->codegen(&reType)) {
      char *fnNameCharss = const_cast<char *>(FnAST->getName().c_str());
      fprintf(
          stderr,
          "Read \033[40;36mfunction Definition\033[0m \033[40;34m%s\033[0m: ",
          fnNameCharss);
      FnIR->print(errs());
      fprintf(stderr, "\n");

      TheJIT->addModule(std::move(TheModule));
      InitializeModuleAndPassManager();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto *FnIR = ProtoAST->codegen()) {
      char *fnNameCharss = const_cast<char *>(ProtoAST->getName().c_str());
      fprintf(stderr, "Read \033[40;36mExtern\033[0m \033[40;34m%s\033[0m: ",
              fnNameCharss);
      FnIR->print(errs());
      fprintf(stderr, "\n");
      FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleVal() {
  if (auto autoValAST = ParseValExpr()) {
    if (auto *ValIR = autoValAST->codegen()) {
      char *valNameCharss = const_cast<char *>(autoValAST->getName().c_str());
      fprintf(stderr, "Read \033[40;36mVal\033[0m \033[40;34m%s\033[0m: ",
              valNameCharss);

      ValIR->print(errs());
      fprintf(stderr, "\n");
    }
  } else {
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  Type *reType = nullptr;
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (auto FnIR = FnAST->codegen(&reType)) {
      fprintf(stderr, "Read \033[40;36mTop\033[0m:");
      // FnIR->print(errs());
      // fprintf(stderr, "\n");

      // JIT the module containing the anonymous expression, keeping a handle so
      // we can free it later.
      auto H = TheJIT->addModule(std::move(TheModule));
      InitializeModuleAndPassManager();

      // Search the JIT for the __anon_expr symbol.
      auto ExprSymbol = TheJIT->findSymbol("__anon_expr");
      assert(ExprSymbol && "Function not found");

      if (reType == Type::getDoubleTy(TheContext)) {
        // Get the symbol's address and cast it to the right type (takes no
        // arguments, returns a double) so we can call it as a native function.
        double (*FP)() =
            (double (*)())(intptr_t)cantFail(ExprSymbol.getAddress());
        Value *F = ConstantFP::get(TheContext, APFloat(FP()));
        NamedValues_Global["it"] = F;

        fprintf(stderr,
                "val \033[40;34mit\033[0m = \033[40;36m%f\033[0m : "
                "\033[40;33mreal\033[0m\n",
                FP());
      } else {
        int (*IP)() = (int (*)())(intptr_t)cantFail(ExprSymbol.getAddress());
        Value *I =
            ConstantInt::get(Type::getInt32Ty(TheContext), (int32_t)IP(), true);
        NamedValues_Global["it"] = I;
        fprintf(stderr,
                "val \033[40;34mit\033[0m = \033[40;36m%d\033[0m : "
                "\033[40;33mint\033[0m\n",
                IP());
      }

      // Delete the anonymous expression module from the JIT.
      TheJIT->removeModule(H);
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
  while (true) {
    fprintf(stderr, "sml->");
    switch (CurTok) {
    case tok_eof:
      return;
    case ';': // ignore top-level semicolons.
      getNextToken();
      break;
    case tok_fun:
      HandleDefinition();
      break;
    case tok_extern:
      HandleExtern();
      break;
    case tok_val:
      HandleVal();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X) {
  fputc((char)X, stderr);
  return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();

  // Install standard binary operators.
  // 1 is lowest precedence.
  BinopPrecedence['='] = 2;
  BinopPrecedence['<'] = 10;
  BinopPrecedence['>'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40; // highest.

  // Prime the first token.
  fprintf(stderr, "sml-> ");
  getNextToken();

  TheJIT = std::make_unique<KaleidoscopeJIT>();

  InitializeModuleAndPassManager();

  // Run the main "interpreter loop" now.
  MainLoop();

  return 0;
}
