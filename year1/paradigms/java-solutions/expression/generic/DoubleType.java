package expression.generic;

public class DoubleType extends Type<Double> {
    @Override
    public Double add(Double x, Double y) {
        return x + y;
    }

    @Override
    public Double subtract(Double x, Double y) {
        return x - y;
    }

    @Override
    public Double multiply(Double x, Double y) {
        return x * y;
    }

    @Override
    public Double divide(Double x, Double y) {
        return x / y;
    }

    @Override
    public Double negate(Double x) {
        return -x;
    }

    @Override
    public Double min(Double x, Double y) {
        return Double.min(x, y);
    }

    @Override
    public Double max(Double x, Double y) {
        return Double.max(x, y);
    }

    @Override
    public int count(Double x) {
        long value = Double.doubleToLongBits(x);
        int count = 0;
        while (value != 0) {
            count += (value % 2 + 2) % 2;
            value >>>= 1;
        }
        return count;
    }

    @Override
    public Double parseConst(String number) {
        return Double.parseDouble(number);
    }

    @Override
    public Double convertTo(int value) {
        return (double) value;
    }
}
