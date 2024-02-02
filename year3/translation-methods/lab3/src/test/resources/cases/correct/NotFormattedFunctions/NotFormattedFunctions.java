public class Program {
    public int f1(int arg0) {
        if (arg0 == 1) {
            return 0;
        }
        return arg0 * 7;
    }

    public boolean f2(boolean arg0) {
        if (arg0 == true) {
            return false;
        }
        return arg0;
    }

    public int f3(int arg0, int arg1) {
        if (arg0 > 0) {
            return 1;
        } else if (arg0 < 0) {
            return -1;
        } else if (arg1 == 0) {
            return arg0 * arg1 - arg0 - arg1;
        }
    }
}