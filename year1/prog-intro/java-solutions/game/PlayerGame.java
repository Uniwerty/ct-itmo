package game;

import java.util.ArrayList;
import java.util.List;

public class PlayerGame {
    private final Board board;
    public final List<PlayerStat> players;

    public PlayerGame(Board board, Player... players) {
        this.board = board;
        this.players = new ArrayList<>();
        for (int i = 0; i < players.length; i++) {
            this.players.add(new PlayerStat(players[i], i + 1));
        }
    }

    public int play(boolean log) {
        while (true) {
            for (int i = 0; i < players.size(); i++) {
                final int result = makeMove(
                        players.get(i).player,
                        players.get(i).number,
                        players.get((i + 1) % players.size()).number,
                        log
                );
                if (result == -2) {
                    i = (i - 1 + players.size()) % players.size();
                    continue;
                }
                if (result != -1) {
                    return result;
                }
            }
        }
    }

    private int makeMove(Player player, int no, int nextPlayer, boolean log) {
        final Move move = player.makeMove(board.getPosition());
        final GameResult result = board.makeMove(move, nextPlayer);
        if (log) {
            System.out.println();
            System.out.println("Player: " + no);
            System.out.println(move);
            System.out.println(board);
            System.out.println("Result: " + result);
        }
        switch (result) {
            case WIN:
                return no;
            case LOSE:
                for (int i = 0; i < players.size(); i++) {
                    if (players.get(i).number == no) {
                        players.remove(i);
                        break;
                    }
                }
                System.out.println("Player " + no + " lost");
                if (players.size() == 1) {
                    return players.get(0).number;
                }
                return -2;
            case DRAW:
                return 0;
            case UNKNOWN:
                return -1;
            default:
                throw new AssertionError("Unknown makeMove result " + result);
        }
    }
}
