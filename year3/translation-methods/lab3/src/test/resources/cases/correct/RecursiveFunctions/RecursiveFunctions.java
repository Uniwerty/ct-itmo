public class Program {
    public int fibonacci(int arg0) {
        if (arg0 == 0) {
            return 1;
        }
        if (arg0 == 1) {
            return 1;
        }
        return fibonacci(arg0 - 1) + fibonacci(arg0 - 2);
    }

    public int ackermann(int arg0, int arg1) {
        if (arg0 == 0) {
            return arg1 + 1;
        } else if (arg0 > 0 && arg1 == 0) {
            return ackermann(arg0 - 1, 1);
        } else if (arg0 > 0 && arg1 > 0) {
            return ackermann(arg0 - 1, ackermann(arg0, arg1 - 1));
        }
    }
}