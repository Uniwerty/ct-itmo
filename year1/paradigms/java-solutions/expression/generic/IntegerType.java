package expression.generic;

import expression.exceptions.DivisionByZeroException;

public class IntegerType extends Type<Integer> {
    @Override
    public Integer add(Integer x, Integer y) {
        return x + y;
    }

    @Override
    public Integer subtract(Integer x, Integer y) {
        return x - y;
    }

    @Override
    public Integer multiply(Integer x, Integer y) {
        return x * y;
    }

    @Override
    public Integer divide(Integer x, Integer y) {
        return x / y;
    }

    @Override
    public Integer negate(Integer x) {
        return -x;
    }

    @Override
    public Integer min(Integer x, Integer y) {
        return Integer.min(x, y);
    }

    @Override
    public Integer max(Integer x, Integer y) {
        return Integer.max(x, y);
    }

    @Override
    public int count(Integer x) {
        int value = x;
        int count = 0;
        while (value != 0) {
            count += (value % 2 + 2) % 2;
            value >>>= 1;
        }
        return count;
    }

    @Override
    public Integer parseConst(String number) {
        return Integer.parseInt(number);
    }

    @Override
    public Integer convertTo(int value) {
        return value;
    }
}