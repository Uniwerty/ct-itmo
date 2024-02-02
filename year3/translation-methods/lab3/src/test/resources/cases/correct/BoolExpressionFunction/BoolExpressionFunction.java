public class Program {
    public boolean xi(int arg0, boolean arg1, int arg2, boolean arg3) {
        if (arg0 == -1 && arg1 == true) {
            return arg2 < 0 || arg3;
        }
        if (arg1 == false && arg3 == false) {
            return arg0 == arg2;
        }
        return (arg0 == arg2) && arg1 || (arg0 != arg2) && arg3 || !(arg1) || !(arg3);
    }
}