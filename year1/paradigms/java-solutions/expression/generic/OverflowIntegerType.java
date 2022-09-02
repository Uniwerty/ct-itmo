package expression.generic;

import expression.exceptions.DivisionByZeroException;
import expression.exceptions.OverflowException;

public class OverflowIntegerType extends Type<Integer> {
    @Override
    public Integer add(Integer x, Integer y) {
        int sum = x + y;
        if (x > 0 && y > 0 && sum <= 0 || x < 0 && y < 0 && sum >= 0) {
            throw new OverflowException("overflow");
        }
        return sum;
    }

    @Override
    public Integer subtract(Integer x, Integer y) {
        int sub = x - y;
        if (x > y && sub < 0 || x < y && sub > 0) {
            throw new OverflowException("overflow");
        }
        return sub;
    }

    @Override
    public Integer multiply(Integer x, Integer y) {
        int mul = x * y;
        if (x > 0 && y > 0 && mul <= 0 || x < 0 && y < 0 && mul <= 0 ||
                x < 0 && y > 0 && mul >= 0 || x > 0 && y < 0 && mul >= 0 ||
                x != 0 && mul / x != y) {
            throw new OverflowException("overflow");
        }
        return mul;
    }

    @Override
    public Integer divide(Integer x, Integer y) {
        int div = x / y;
        if (x > 0 && y > 0 && div < 0 || x < 0 && y < 0 && div < 0 ||
                x < 0 && y > 0 && div > 0 || x > 0 && y < 0 && div > 0) {
            throw new OverflowException("overflow");
        }
        return div;
    }

    @Override
    public Integer negate(Integer x) {
        int negate = -x;
        if (x > 0 && negate > 0 || x < 0 && negate < 0) {
            throw new OverflowException("overflow");
        }
        return negate;
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