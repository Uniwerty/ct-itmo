public class Program {
    public int f(int arg0, int arg1) {
        return arg0 / arg1;
    }

    public int g(int arg0, int arg1) {
        return f(arg0, arg1) * f(arg1, arg0);
    }

    public int h(int arg0, int arg1) {
        return f(arg0, f(arg0, arg1)) - g(g(arg1, arg0), arg0);
    }
}