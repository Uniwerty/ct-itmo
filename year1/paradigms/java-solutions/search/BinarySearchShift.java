package search;

public class BinarySearchShift {
    // Pred: args.length > 0
    // Post: array[k - 1] > a[k] && array[k + 1] > a[k]
    public static void main(String[] args) {
        int[] array = new int[args.length];
        for (int i = 0; i < args.length; i++) {
            array[i] = Integer.parseInt(args[i]);
        }
        int k = binarySearch(array);
        System.out.println(k);
    }

    // Pred: ∃ k: (∀ i != k && 0 < i < array.length: array[i - 1] < array[i]) &&  (array[k - 1] > array[k] && array[k + 1] > array[k])
    // Post: V == k
    public static int binarySearch(int[] array) {
        // true
        int l = -1;
        // l == -1
        int r = array.length;
        // r == array.length
        int n = array.length - 1;
        // n == array.length - 1
        // I: l == -1 || r == array.length || (array[l] > array[n] && array[r] < array[n])
        while (r - l > 1) {
            // array[l] > array[n] && array[r] < array[n] && r - l > 1
            // array[l] > array[n] && array[r] < array[n] && r > l + 1 && l < r - 1
            // array[l] > array[n] && array[r] < array[n] && (l + r) / 2 > (2 * l + 1) / 2 && (l + r) / 2 < (2 * r - 1) / 2
            // array[l] > array[n] && array[r] < array[n] && (l + r) / 2 > l && (l + r) / 2 < r - 1
            int m = (l + r) / 2;
            // array[l] > array[n] && array[r] < array[n] && (l + r) / 2 > l && (l + r) / 2 < r - 1 && m == (l + r) / 2
            // P: array[l] > array[n] && array[r] < array[n] && m > l && m < r - 1
            if (array[m] <= array[n]) {
                // P && cond -> P1
                // P1: array[l] > array[n] && array[r] < array[n] && m > l && m < r - 1 && array[m] <= array[n]
                r = m;
                // array[l] > array[n] && array[r] < array[n] && m > l && m < r - 1 && array[m] <= array[n] && r' == m && l' == l && array[m] != array[n]
                // Q1: array[l'] > array[n] && array[r'] < array[n] && r' > l && r' < r - 1 && l' == l
                // Q1 -> Q
            } else {
                // P && !cond -> P2
                // P2: array[l] > array[n] && array[r] < array[n] && m > l && m < r - 1 && array[m] > array[n]
                l = m;
                // array[l] > array[n] && array[r] < array[n] && m > l && m < r - 1 && array[m] > array[n] && l' == m && r' == r
                // Q2: array[l'] > array[n] && array[r'] < array[n] && l' > l && l' < r - 1 && r' == r
                // Q2 -> Q
            }
            // Q: array[l'] > array[n] && array[r'] < array[n]
            // Q -> I
        }
        // I && !cond: (l == -1 || r == array.length || (array[l] > array[n] && array[r] < array[n])) && r - l <= 1
        // return V
        return l + 1;
    }
}