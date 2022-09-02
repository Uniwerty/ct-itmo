package game;

import java.util.Scanner;

public class HumanPlayer implements Player {
    private final Scanner in;

    public HumanPlayer(Scanner in) {
        this.in = in;
    }

    @Override
    public Move makeMove(Position position) {
        System.out.println();
//        System.out.println("Current position");
//        System.out.println(position);
        System.out.println("Enter your move for " + position.getTurn());
        Move move;
        while (true) {
            String newRow = in.next();
            String newCol = in.next();
            if (isNumeric(newRow) && isNumeric(newCol)) {
                move = new Move(Integer.parseInt(newRow) - 1, Integer.parseInt(newCol) - 1, position.getTurn());
                if (position.isValid(move)) {
                    return move;
                }
            }
            System.out.println("This move is invalid. Enter new move: ");
        }
    }

    private boolean isNumeric(String string) {
        try {
            Integer.parseInt(string);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }
}
