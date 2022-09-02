package expression.generic;

public class LongType extends Type<Long> {
    @Override
    public Long add(Long x, Long y) {
        return x + y;
    }

    @Override
    public Long subtract(Long x, Long y) {
        return x - y;
    }

    @Override
    public Long multiply(Long x, Long y) {
        return x * y;
    }

    @Override
    public Long divide(Long x, Long y) {
        return x / y;
    }

    @Override
    public Long negate(Long x) {
        return -x;
    }

    @Override
    public Long min(Long x, Long y) {
        return Long.min(x, y);
    }

    @Override
    public Long max(Long x, Long y) {
        return Long.max(x, y);
    }

    @Override
    public int count(Long x) {
        long value = x;
        int count = 0;
        while (value != 0) {
            count += (value % 2 + 2) % 2;
            value >>>= 1;
        }
        return count;
    }

    @Override
    public Long parseConst(String number) {
        return Long.parseLong(number);
    }

    @Override
    public Long convertTo(int value) {
        return (long) value;
    }
}
