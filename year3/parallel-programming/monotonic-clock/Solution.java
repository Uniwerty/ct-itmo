import org.jetbrains.annotations.NotNull;

/**
 * В теле класса решения разрешено использовать только финальные переменные типа RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author Ivchenkov Dmitrii
 */
public class Solution implements MonotonicClock {
    private final RegularInt c11 = new RegularInt(0);
    private final RegularInt c12 = new RegularInt(0);
    private final RegularInt c13 = new RegularInt(0);

    private final RegularInt c21 = new RegularInt(0);
    private final RegularInt c22 = new RegularInt(0);
    private final RegularInt c23 = new RegularInt(0);

    @Override
    public void write(@NotNull Time time) {
        c11.setValue(time.getD1());
        c12.setValue(time.getD2());
        c13.setValue(time.getD3());

        c23.setValue(c13.getValue());
        c22.setValue(c12.getValue());
        c21.setValue(c11.getValue());
    }

    @NotNull
    @Override
    public Time read() {
        int r11 = c11.getValue();
        int r12 = c12.getValue();
        int r13 = c13.getValue();

        int r23 = c23.getValue();
        int r22 = c22.getValue();
        int r21 = c21.getValue();

        if (r11 == r21 && r12 == r22 && r13 == r23) {
            return new Time(r11, r12, r13);
        } else {
            int t1;
            int t2;
            int t3;
            if (r11 == r21) {
                t1 = r11;
                if (r12 == r22) {
                    t2 = r12;
                    t3 = getNumber(r13, r23);
                } else {
                    t2 = getNumber(r12, r22);
                    t3 = Integer.MAX_VALUE;
                }
            } else {
                t1 = getNumber(r11, r21);
                t2 = Integer.MAX_VALUE;
                t3 = Integer.MAX_VALUE;
            }
            return new Time(t1, t2, t3);
        }
    }

    private int getNumber(int x, int y) {
        String xString = Integer.toBinaryString(x);
        String yString = Integer.toBinaryString(y);
        int i = 0;
        while (i < xString.length() && xString.charAt(i) == yString.charAt(i)) {
            i++;
        }
        String firstPart = xString.substring(0, i + 1);
        return Integer.parseInt(firstPart + "1".repeat(32 - firstPart.length()), 2);
    }
}
