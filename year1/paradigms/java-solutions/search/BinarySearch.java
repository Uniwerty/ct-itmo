package search;

public class BinarySearch {
    // Pred: args.length > 0
    // Post: array[ans] <= x && (ans == 0 || ans > 0 && array[ans - 1] > x)
    public static void main(String[] args) {
        int[] array = new int[args.length - 1];
        for (int i = 1; i < args.length; i++) {
            array[i - 1] = Integer.parseInt(args[i]);
        }
        int x = Integer.parseInt(args[0]);
        int ans = iterativeBinarySearch(array, x);
        // int ans = recursiveBinarySearch(array, -1, array.length, x);
        System.out.println(ans);
    }

    // Pred: ∀ 0 < i < array.length: array[i - 1] >= array[i]
    // Post: array[R] <= x && (R == 0 || R > 0 && array[R - 1] > x)
    public static int iterativeBinarySearch(int[] array, int x) {
        // true
        int l = -1;
        // l == -1
        // true
        int r = array.length;
        // r == array.length
        // :NOTE: no finishing statement in the invariant
        // I: l == -1 || r == array.length || (array[l] > x && array[r] <= x)
        while (r - l > 1) {
            // (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && r - l > 1
            // (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && r > l + 1 && l < r - 1
            // (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && (l + r) / 2 > (2 * l + 1) / 2 && (l + r) / 2 < (2 * r - 1) / 2
            // (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && (l + r) / 2 > l && (l + r) / 2 < r - 1
            int m = (l + r) / 2;
            // (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && (l + r) / 2 > l && (l + r) / 2 < r - 1 && m == (l + r) / 2
            // P: (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && m > l && m < r - 1
            if (array[m] <= x) {
                // P && cond -> P1
                // P1: (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && m > l && m < r - 1 && array[m] <= x
                r = m;
                // (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && m > l && m < r - 1 && array[m] <= x && r' == m && l' == l
                // Q1: (l' == -1 || r' == array.length || (array[l'] > x && array[r'] <= x)) && r' > l && r' < r - 1 && l' == l
                // Q1 -> Q
            } else {
                // P && !cond -> P2
                // P2: (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && m > l && m < r - 1 && array[m] > x
                l = m;
                // (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && m > l && m < r - 1 && array[m] > x && l' == m && r' == r
                // Q2: (l' == -1 || r' == array.length || (array[l'] > x && array[r'] <= x)) && l' > l && l' < r - 1 && r' == r
                // Q2 -> Q
            }
            // Q: l' == -1 || r' == array.length || (array[l'] > x && array[r'] <= x)
            // Q -> I
        }
        // I && !cond: (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && r - l <= 1
        return r;
    }

    // :NOTE: pre-condition too strict
    // Pred: ∀ 0 < i < array.length: array[i - 1] >= array[i] && (l == -1 || r == array.length || (array[l] > x && array[r] <= x))
    // :NOTE: post-condition too soft
    // Post: array[R] <= x
    public static int recursiveBinarySearch(int[] array, int l, int r, int x) {
        // P: ∀ 0 < i < array.length: array[i - 1] >= array[i] && (l == -1 || r == array.length || (array[l] > x && array[r] <= x))
        if (r - l <= 1) {
            // P && cond -> P1
            // P1: (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && r - l <= 1
            return r;
        } else {
            // P && !cond -> P2
            // P2: (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && r - l > 1
            // (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && r > l + 1 && l < r - 1
            // (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && (l + r) / 2 > (2 * l + 1) / 2 && (l + r) / 2 < (2 * r - 1) / 2
            // (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && (l + r) / 2 > l && (l + r) / 2 < r - 1
            int m = (l + r) / 2;
            // (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && (l + r) / 2 > l && (l + r) / 2 < r - 1 && m == (l + r) / 2
            // P': (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && m > l && m < r - 1
            if (array[m] <= x) {
                // P' && cond' -> P1'
                // P1': (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && m > l && m < r - 1 && array[m] <= x
                r = m;
                // (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && m > l && m < r - 1 && array[m] <= x && r' = m && l' = l
                // Q1': (l' == -1 || r' == array.length || (array[l'] > x && array[r'] <= x)) && r' > l && r' < r - 1 && l' == l
                // Q1' -> Q'
            } else {
                // P' && !cond' -> P2'
                // P2': (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && m > l && m < r - 1 && array[m] > x
                l = m;
                // (l == -1 || r == array.length || (array[l] > x && array[r] <= x)) && m > l && m < r - 1 && array[m] > x && l' == m && r' == r
                // Q2': (l' == -1 || r' == array.length || (array[l'] > x && array[r'] <= x)) && l' > l && l' < r - 1 && r' == r
                // Q2' -> Q'
            }
            // Q': l' == -1 || r' == array.length || array[l'] > x && array[r'] <= x
            // Q' -> Pred
            return recursiveBinarySearch(array, l, r, x);
        }
    }
}