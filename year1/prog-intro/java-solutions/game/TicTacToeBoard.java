package game;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class TicTacToeBoard implements Board, Position {
    private static final Map<Cell, String> CELL_TO_STRING = Map.of(
            Cell.E, ".",
            Cell.X, "X",
            Cell.O, "0",
            Cell.M, "-",
            Cell.L, "|"
    );
    private final List<Cell> turns = List.of(Cell.X, Cell.O, Cell.M, Cell.L);;
    private final List<ArrayList<Cell>> field;
    private Cell turn;
    public final int m;
    public final int n;
    public final int k;

    public TicTacToeBoard(int m, int n, int k) {
        this.m = m;
        this.n = n;
        this.k = k;
        field = new ArrayList<>();
        for (int i = 0; i < m; i++) {
            field.add(new ArrayList<>());
            for (int j = 0; j < n; j++) {
                field.get(i).add(Cell.E);
            }
        }
        turn = Cell.X;
    }

    @Override
    public Cell getTurn() {
        return turn;
    }

    @Override
    public Position getPosition() {
        return this;
    }

    @Override
    public GameResult makeMove(Move move, int nextPlayer) {
        if (!isValid(move)) {
            turn = turns.get(nextPlayer - 1);
            return GameResult.LOSE;
        }
        field.get(move.getRow()).set(move.getCol(), move.getValue());
        if (checkPosition(move.getRow(), move.getCol())) {
            return GameResult.WIN;
        }
        if (checkDraw()) {
            return GameResult.DRAW;
        }
        turn = turns.get(nextPlayer - 1);
        return GameResult.UNKNOWN;
    }

    private boolean checkPosition(int row, int col) {
//        for (int i = 0; i < m; i++) {
//            for (int j = 0; j < n; j++) {
//                // check column
//                if (i + k <= m) {
//                    for (int l = i; l < i + k; l++) {
//
//                    }
//                }
//                // check row
//                if (j + k <= n) {
//                    for (int l = j; l < j + k; l++) {
//
//                    }
//                }
//                // check main diagonal
//                if (i + k <= m && j + k <= n) {
//                    for (int l = i; l < i + k; l++) {
//
//                    }
//                }
//            }
//        }

//        return col >= k - 1 && checkRow(row, col - k + 1) ||
//                col <= n - k && checkRow(row, col) ||
//                row >= k - 1 && checkCol(row - k + 1, col) ||
//                row <= n - k && checkCol(row, col) ||
//                row >= k - 1 && col >= k - 1 && checkDiagonal(row - k + 1, true) ||
//                row <= n - k && col <= n - k && checkDiagonal(row, true) ||
//                row >= k - 1 && col <= n - k && checkDiagonal(row - k + 1, false) ||
//                row <= n - k && col >= k - 1 && checkDiagonal(row, false);
        return checkRow(row, col) || checkCol(row, col) || checkMainDiagonal(row, col) || checkSideDiagonal(row, col);
    }

    private boolean checkRow(int row, int col) {
        int l = col;
        int r = col;
        while (l >= 0 && field.get(row).get(l) == turn) {
            l--;
        }
        while (r < n && field.get(row).get(r) == turn) {
            r++;
        }
        return k <= r - l - 1;
//        for (int j = col; j < col + k; j++) {
//            if (field.get(row).get(j) != turn) {
//                return false;
//            }
//        }
//        return true;
    }

    private boolean checkCol(int row, int col) {
        int up = row;
        int down = row;
        while (up >= 0 && field.get(up).get(col) == turn) {
            up--;
        }
        while (down < m && field.get(down).get(col) == turn) {
            down++;
        }
        return k <= down - up - 1;
//        for (int i = row; i < row + k; i++) {
//            if (field.get(i).get(col) != turn) {
//                return false;
//            }
//        }
//        return true;
    }

//    private boolean checkDiagonal(int row, boolean isMain) {
//        for (int i = row; i < row + k; i++) {
//            int j = isMain ? i : n - i - 1;
//            if (field.get(i).get(j) != turn) {
//                return false;
//            }
//        }
//        return true;
//    }
    private boolean checkMainDiagonal(int row, int col) {
        int leftUpRow = row;
        int leftUpCol = col;
        int rightDownRow = row;
        int rightDownCol = col;
        while (leftUpRow >= 0 && leftUpCol >= 0 && field.get(leftUpRow).get(leftUpCol) == turn) {
            leftUpRow--;
            leftUpCol--;
        }
        while (rightDownRow < m && rightDownCol < n && field.get(rightDownRow).get(rightDownCol) == turn) {
            rightDownRow++;
            rightDownCol++;
        }
        return k <= rightDownRow - leftUpRow - 1;
    }

    private boolean checkSideDiagonal(int row, int col) {
        int leftDownRow = row;
        int leftDownCol = col;
        int rightUpRow = row;
        int rightUpCol = col;
        while (leftDownRow < m && leftDownCol >= 0 && field.get(leftDownRow).get(leftDownCol) == turn) {
            leftDownRow++;
            leftDownCol--;
        }
        while (rightUpRow >= 0 && rightUpCol < n && field.get(rightUpRow).get(rightUpCol) == turn) {
            rightUpRow--;
            rightUpCol++;
        }
        return k <= leftDownRow - rightUpRow - 1;
    }

    private boolean checkDraw() {
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                if (field.get(i).get(j) == Cell.E) {
                    return false;
                }
            }
        }
        return true;
    }

    public boolean isValid(final Move move) {
        return 0 <= move.getRow() && move.getRow() < m
                && 0 <= move.getCol() && move.getCol() < n
                && field.get(move.getRow()).get(move.getCol()) == Cell.E
                && turn == move.getValue();
    }

    @Override
    public Cell getCell(int row, int column) {
        return field.get(row).get(column);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("   ");
        for (int c = 1; c <= n; c++) {
            sb.append(String.format("%3d", c));
        }
        sb.append(System.lineSeparator());
        for (int r = 0; r < m; r++) {
            sb.append(String.format("%3d", r + 1));
            for (Cell cell : field.get(r)) {
                sb.append(String.format("%3s", CELL_TO_STRING.get(cell)));
            }
            sb.append(System.lineSeparator());
        }
        sb.setLength(sb.length() - System.lineSeparator().length());
        return sb.toString();
    }
}
