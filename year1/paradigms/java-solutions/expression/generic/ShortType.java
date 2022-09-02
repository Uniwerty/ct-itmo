package expression.generic;

import expression.exceptions.DivisionByZeroException;

public class ShortType extends Type<Short> {
    @Override
    public Short add(Short x, Short y) {
        return (short) (x + y);
    }

    @Override
    public Short subtract(Short x, Short y) {
        return (short) (x - y);
    }

    @Override
    public Short multiply(Short x, Short y) {
        return (short) (x * y);
    }

    @Override
    public Short divide(Short x, Short y) {
        return (short) (x / y);
    }

    @Override
    public Short negate(Short x) {
        return (short) -x;
    }

    @Override
    public Short min(Short x, Short y) {
        return x < y ? x : y;
    }

    @Override
    public Short max(Short x, Short y) {
        return x > y ? x : y;
    }

    @Override
    public int count(Short x) {
        int value = Short.toUnsignedInt(x);
        int count = 0;
        while (value != 0) {
            count += (value % 2 + 2) % 2;
            value >>>= 1;
        }
        return count;
    }

    @Override
    public Short parseConst(String number) {
        return (short) Integer.parseInt(number);
    }

    @Override
    public Short convertTo(int value) {
        return (short) value;
    }
}
