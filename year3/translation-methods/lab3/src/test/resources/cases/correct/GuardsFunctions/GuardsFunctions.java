public class Program {
    public int kappa(int arg0, int arg1) {
        if (arg0 == 0 && arg1 == 0) {
            return -1;
        }
        if (arg0 == arg1) {
            return arg0 * arg1;
        } else if (arg0 == arg1 && arg0 != 0) {
            return arg0 / arg1;
        } else if (true) {
            return 1000000;
        }
    }

    public int lambda(boolean arg0, boolean arg1) {
        if (arg0 == true) {
            return 2;
        }
        if (arg0 || arg1) {
            return 1;
        }
        return 15;
    }

    public int epsilon(int arg0, boolean arg1) {
        if (arg0 < 0 && arg1) {
            return arg0;
        } else if (arg0 > 0 && !(arg1)) {
            return 0;
        }
        if (arg0 == 8 && arg1 == false) {
            return 9;
        }
        return arg0;
    }

    public boolean upsilon(int arg0, int arg1) {
        if (arg0 == 0 && arg1 == 0) {
            if (1 < 0) {
                return 1 < 0;
            } else if (1 > 0) {
                return true;
            }
        }
        if (arg0 == arg1) {
            return arg0 > arg1;
        } else if (arg1 != arg0) {
            return arg1 > arg0;
        }
    }
}