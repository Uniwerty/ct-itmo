let cnst = value => (x, y, z) => value;
let variable = sign => (x, y, z) => {
    switch (sign) {
        case "x": return x;
        case "y": return y;
        case "z": return z;
        default: return undefined;
    }
};
let binaryOperation = operation => (first, second) => (x, y, z) => operation(first(x, y, z), second(x, y, z));
let addOp = (a, b) => a + b;
let subOp = (a, b) => a - b;
let mulOp = (a, b) => a * b;
let divOp = (a, b) => a / b;

let add = binaryOperation(addOp);
let subtract = binaryOperation(subOp);
let multiply = binaryOperation(mulOp);
let divide = binaryOperation(divOp);

let unaryOperation = operation => (expression) => (x, y, z) => operation(expression(x, y, z));
let negation = a => -a;

let negate = unaryOperation(negation);
let sinh = unaryOperation(Math.sinh);
let cosh = unaryOperation(Math.cosh);

let pi = () => Math.PI;
let e = () => Math.E;

let getBinaryFunction = (operator) => (first, second) => {
    switch (operator) {
        case "+": return add(first, second);
        case "-": return subtract(first, second);
        case "*": return multiply(first, second);
        case "/": return divide(first, second);
        default: return undefined;
    }
}
let getUnaryFunction = (operator, expression) => {
    switch (operator) {
        case "negate": return negate(expression);
        case "sinh": return sinh(expression);
        case "cosh": return cosh(expression);
        default: return undefined;
    }
}
let parse = function (expression) {
    let operands = [];
    let i = 0;
    expression = expression.trim();
    while (i < expression.length) {
        while (expression.charAt(i) === " " && i < expression.length) {
            i++;
        }
        let j = i;
        while (expression.charAt(i) !== " " && i < expression.length) {
            i++;
        }
        let token = expression.substring(j, i);
        if (token === "+" || token === "-" || token === "*" || token === "/") {
            let second = operands.pop();
            let first = operands.pop();
            operands.push(getBinaryFunction(token, first, second));
        } else if (token === "negate" || token === "sinh" || token === "cosh") {
            operands.push(getUnaryFunction(token, operands.pop()));
        } else if (token === "pi") {
            operands.push(pi);
        } else if (token === "e") {
            operands.push(e);
        } else if (token === "x" || token === "y" || token === "z") {
            operands.push(variable(token));
        } else {
            operands.push(cnst(parseFloat(token)));
        }
    }
    return operands[0];
}