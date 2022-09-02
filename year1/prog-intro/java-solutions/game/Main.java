package game;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        //Scanner in = new Scanner(System.in);
        int m = Integer.parseInt(args[0]);//in.nextInt();
        int n = Integer.parseInt(args[1]);//in.nextInt();
        int k = Integer.parseInt(args[2]);//in.nextInt();
        final int result = new PlayerGame(
                new TicTacToeBoard(m, n, k),
                new RandomPlayer(),
                new RandomPlayer(),
                new RandomPlayer(),
                new RandomPlayer()
        ).play(true);
        switch (result) {
            case 1:
                System.out.println("First player won");
                break;
            case 2:
                System.out.println("Second player won");
                break;
            case 3:
                System.out.println("Third player won");
                break;
            case 4:
                System.out.println("Fourth player won");
                break;
            case 0:
                System.out.println("Draw");
                break;
            default:
                throw new AssertionError("Unknown result " + result);
        }
    }
}
