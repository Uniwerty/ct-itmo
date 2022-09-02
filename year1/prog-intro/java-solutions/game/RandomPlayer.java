package game;

import java.util.Random;

public class RandomPlayer implements Player {
    private final Random random = new Random();
    @Override
    public Move makeMove(Position position) {
        final TicTacToeBoard board = (TicTacToeBoard) position;
        while (true) {
            final Move move = new Move(
                    random.nextInt(board.m),
                    random.nextInt(board.n),
                    position.getTurn()
            );
            if (position.isValid(move)) {
                return move;
            }
        }
    }
}
