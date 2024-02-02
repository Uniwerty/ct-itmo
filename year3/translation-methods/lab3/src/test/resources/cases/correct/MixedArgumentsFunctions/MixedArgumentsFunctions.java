public class Program {
    public int eta(int arg0, int arg1) {
        if (arg0 == 1 && arg1 == 1) {
            return 1;
        }
        if (arg1 == 0) {
            return arg0;
        }
        if (arg0 == 0) {
            return 0;
        }
        return arg0 + arg1;
    }

    public boolean zeta(boolean arg0, boolean arg1) {
        if (arg0 == true && arg1 == true) {
            return true;
        }
        return arg0 && !(arg1);
    }

    public boolean theta(int arg0, int arg1) {
        if (arg0 == 0 && arg1 == 1) {
            return (0 < 1);
        }
        if (arg0 == 1 && arg1 == 0) {
            return 1 < 0;
        }
        if (arg0 == 2 && arg1 == 2) {
            return true;
        }
        return false || arg0 != arg1;
    }
}