package game;

public class SequentialPlayer implements Player {
    @Override
    public Move makeMove(Position position) {
        final TicTacToeBoard board = (TicTacToeBoard) position;
        for (int r = 0; r < board.m; r++) {
            for (int c = 0; c < board.n; c++) {
                final Move move = new Move(r, c, position.getTurn());
                if (position.isValid(move)) {
                    return move;
                }
            }
        }
        throw new AssertionError("No valid moves");
    }
}
