"use strict"
function BinaryOperation(first, second, operation, operator) {
    this.first = first;
    this.second = second;
    this.operation = operation;
    this.operator = operator;
}

BinaryOperation.prototype.evaluate = function (x, y, z) {
    return this.operation(this.first.evaluate(x, y, z), this.second.evaluate(x, y, z));
}
BinaryOperation.prototype.toString = function () {
    return this.first.toString() + " " + this.second.toString() + " " + this.operator;
}
BinaryOperation.prototype.prefix = function () {
    return "(" + this.operator + " " + this.first.prefix() + " " + this.second.prefix() + ")";
}

function Add(first, second) {
    BinaryOperation.call(this, first, second, (a, b) => a + b, "+");
}

Add.prototype = BinaryOperation.prototype;

function Subtract(first, second) {
    BinaryOperation.call(this, first, second, (a, b) => a - b, "-");
}

Subtract.prototype = BinaryOperation.prototype;

function Multiply(first, second) {
    BinaryOperation.call(this, first, second, (a, b) => a * b, "*");
}

Multiply.prototype = BinaryOperation.prototype;

function Divide(first, second) {
    BinaryOperation.call(this, first, second, (a, b) => a / b, "/");
}

Divide.prototype = BinaryOperation.prototype;

function UnaryOperation(expression, operation, operator) {
    this.expression = expression;
    this.operation = operation;
    this.operator = operator;
}

UnaryOperation.prototype.evaluate = function (x, y, z) {
    return this.operation(this.expression.evaluate(x, y, z));
}
UnaryOperation.prototype.toString = function () {
    return this.expression.toString() + " " + this.operator;
}
UnaryOperation.prototype.prefix = function () {
    return "(" + this.operator + " " + this.expression.prefix() + ")";
}

function Negate(expression) {
    UnaryOperation.call(this, expression, a => -a, "negate");
}

Negate.prototype = UnaryOperation.prototype;

function Sinh(expression) {
    UnaryOperation.call(this, expression, Math.sinh, "sinh");
}

Sinh.prototype = UnaryOperation.prototype;

function Cosh(expression) {
    UnaryOperation.call(this, expression, Math.cosh, "cosh");
}

Cosh.prototype = UnaryOperation.prototype;

function Const(value) {
    this.value = value;
}

Const.prototype.evaluate = function () {
    return this.value;
}
Const.prototype.toString = function () {
    return this.value.toString();
}
Const.prototype.prefix = function () {
    return this.toString();
}

function Variable(sign) {
    this.sign = sign;
}

Variable.prototype.evaluate = function (x, y, z) {
    switch (this.sign) {
        case "x":
            return x;
        case "y":
            return y;
        case "z":
            return z;
    }
}
Variable.prototype.toString = function () {
    return this.sign;
}
Variable.prototype.prefix = function () {
    return this.toString();
}

let getBinaryOperation = (operator) => (first, second) => {
    switch (operator) {
        case "+":
            return new Add(first, second);
        case "-":
            return new Subtract(first, second);
        case "*":
            return new Multiply(first, second);
        case "/":
            return new Divide(first, second);
    }
}
let getUnaryOperation = (operator) => (expression) => {
    switch (operator) {
        case "negate":
            return new Negate(expression);
        case "sinh":
            return new Sinh(expression);
        case "cosh":
            return new Cosh(expression);
    }
}
let getMultiOperation = (operator) => (...expressions) => {
    switch (operator) {
        case "mean":
            return new Mean(...expressions);
        case "var":
            return new Var(...expressions);
    }
}

let parse = function (expression) {
    let pos = 0;
    let operands = [];
    let skipWhitespace = function () {
        while (expression.charAt(pos) === " " && pos < expression.length) {
            pos++;
        }
    }
    let getToken = function () {
        let start = pos;
        while (pos < expression.length && expression.charAt(pos) !== " ") {
            pos++;
        }
        return expression.substring(start, pos);
    }
    while (pos < expression.length) {
        skipWhitespace();
        let token = getToken();
        if (getBinaryOperation(token)()) {
            let second = operands.pop();
            let first = operands.pop();
            operands.push(getBinaryOperation(token)(first, second));
        } else if (getUnaryOperation(token)()) {
            operands.push(getUnaryOperation(token)(operands.pop()));
        } else if (token === "x" || token === "y" || token === "z") {
            operands.push(new Variable(token));
        } else {
            operands.push(new Const(parseFloat(token)));
        }
        skipWhitespace();
    }
    return operands[0];
}

// Homework 8: Prefix

function MultiOperation(operator, operation, expressions) {
    this.operator = operator;
    this.operation = operation;
    this.expressions = expressions;
    // this.args = arguments;
}
MultiOperation.prototype.evaluate = function (x, y, z) {
    let args = [];
    for (const arg of this.expressions) {
        args.push(arg.evaluate(x, y, z));
    }
    return this.operation(args);
}
MultiOperation.prototype.toString = function () {
    let result = ""
    for (const arg of this.expressions) {
        result += arg.toString();
        result += " ";
    }
    result += this.operator;
    return result;
}
MultiOperation.prototype.prefix = function () {
    let result = "(" + this.operator;
    for (const arg of this.expressions) {
        result += " ";
        result += arg.prefix();
    }
    result += ")";
    return result;
}

let meanOp = (args) => {
    if (args.length === 0) {
        return 0;
    }
    let sum = 0;
    for (const arg of args) {
        sum += arg;
    }
    return sum / args.length;
}
function Mean(...expressions) {
    let args = expressions;
    MultiOperation.call(this, "mean", meanOp, args);
}
Mean.prototype = MultiOperation.prototype;

let varOp = (args) => {
    let args2 = [];
    for (let i = 0; i < args.length; i++) {
        args2[i] = args[i] * args[i];
    }
    return meanOp(args2) - meanOp(args) * meanOp(args);
}
function Var(...expressions) {
    let args = expressions;
    MultiOperation.call(this, "var", varOp, args);
}
Var.prototype = MultiOperation.prototype;

function ParseError(message) {
    Error.call(this, message);
    this.message = message;
}
ParseError.prototype = Error.prototype;
ParseError.prototype.name = "ParseError";
ParseError.prototype.constructor = ParseError;

let parsePrefix = function (expression) {
    if (expression.length === 0) {
        throw new ParseError("Empty input");
    }
    let pos = 0;
    let getToken = function () {
        let start = pos;
        while (pos < expression.length && expression.charAt(pos) !== " " && expression.charAt(pos) !== "(" && expression.charAt(pos) !== ")") {
            pos++;
        }
        return expression.substring(start, pos);
    }
    let skipWhitespace = function () {
        while (pos < expression.length && expression.charAt(pos) === " ") {
            pos++;
        }
    }
    let parseExpression = function () {
        skipWhitespace();
        if (expression.charAt(pos) !== "(") {
            let token = getToken();
            if (token === "x" || token === "y" || token === "z") {
                return new Variable(token);
            } else if (parseFloat(token).toString() === token) {
                return new Const(parseFloat(token));
            } else {
                if (token.length === 1) {
                    throw new ParseError("Unknown variable");
                } else {
                    throw new ParseError("Invalid number");
                }
            }
        } else {
            pos++;
            skipWhitespace();
            let operator = getToken();
            skipWhitespace();
            let operand;
            if (getUnaryOperation(operator)()) {
                try {
                    let argument = parseExpression();
                    operand = getUnaryOperation(operator)(argument);
                } catch (e) {
                    throw new ParseError("Invalid argument");
                }
            } else if (getBinaryOperation(operator)()) {
                try {
                    let first = parseExpression();
                    skipWhitespace();
                    let second = parseExpression();
                    operand = getBinaryOperation(operator)(first, second);
                } catch(e) {
                    throw new ParseError("Invalid argument");
                }
            } else if (getMultiOperation(operator)()) {
                try {
                    let args = [];
                    while (pos < expression.length && expression.charAt(pos) !== ")") {
                        args.push(parseExpression());
                        skipWhitespace();
                    }
                    operand = getMultiOperation(operator)(...args);
                } catch(e) {
                    throw new ParseError("Invalid argument");
                }
            } else {
                throw new ParseError("Unknown operation");
            }
            skipWhitespace();
            if (getToken().length > 0) {
                throw new ParseError("Invalid argument");
            }
            if (expression.charAt(pos) !== ")") {
                throw new ParseError("Missing ')'");
            }
            pos++;
            skipWhitespace();
            return operand;
        }
    }
    let result = parseExpression();
    skipWhitespace();
    if (pos < expression.length) {
        throw new ParseError("Extra characters left");
    }
    return result;
}
console.log(parsePrefix("(mean    7   6     )").prefix())