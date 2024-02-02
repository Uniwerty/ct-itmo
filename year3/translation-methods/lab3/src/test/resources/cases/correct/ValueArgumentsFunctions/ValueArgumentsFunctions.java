public class Program {
    public int sum(int arg0, int arg1) {
        if (arg0 == 0 && arg1 == 0) {
            return 0;
        }
        if (arg0 == 0 && arg1 == 1) {
            return 1;
        }
        if (arg0 == -1 && arg1 == 1) {
            return 0;
        }
    }

    public boolean xor(boolean arg0, boolean arg1) {
        if (arg0 == false && arg1 == false) {
            return false;
        }
        if (arg0 == false && arg1 == true) {
            return true;
        }
        if (arg0 == true && arg1 == false) {
            return true;
        }
        if (arg0 == true && arg1 == true) {
            return false;
        }
    }

    public int mix(boolean arg0, int arg1) {
        if (arg0 == true && arg1 == 0) {
            return 0;
        }
        if (arg0 == true && arg1 == 1) {
            return 1;
        }
        if (arg0 == false && arg1 == 0) {
            return 0;
        }
        if (arg0 == false && arg1 == 1) {
            return 0;
        }
    }
}