####### Protocol #######
#
# a PlayerID is a Symbol
# a Hand is a [List-of Card]
# a Score is a Nat
# a Scores is a [Hash-of PlayerID Score]
# a RoundNo is a Nat
#
# a Round is a {:round, RoundNo, [List-of Card], [List-of Row], PID}
# a PlayerInfo is a {:player, PlayerID, PID}
# a DeclaredWinners is a {:declared_winners, [List-of PlayerID]}
#
# There are two roles in the protocol:
# 1. The Dealer, who manages a run of the game. There is one dealer per game instance.
# 2. The Player, who attempts to win the game by communicating with the Dealer. There
#    are between 2 and 10 players per game.
# 3. The Game Observer, who monitors and listens for the results of the game. There is
#    one Game Observer per game instance.
#
# Conversations:
# There is a conversation about playing one round of the game. The Dealer sends each
# participating player a Round message, containing the round number, the cards in that
# Player's hand, the current rows of the game, and the Dealer's PID. Player's respond 
# by sending a Move message to the Dealer's PID, containing the round number of the
# Round message, the PlayerID of the Player, and the card that the Player has selected
# to play for the round.
#
# The Dealer manages 10 rounds in this way.
# 
# There is a conversation about the results of the game. When the game ends, the Dealer
# sends a DeclaredWinners message to the PID of the Game Observer, containing the PlayerIDs
# of the Players with the lowest scores in the game.

# the Dealer that administers and manages the game for Players
defmodule Dealer do
  # [List-of PlayerInfo] [PID of DeclaredWinners] -> PID
  def spawn(players, game_results_pid) do
    if length(players) < 2 or length(players) > 10 do
      raise "Take-5 is played with 2-10 players"
    else
      spawn fn -> initialize_game(players, game_results_pid) end
    end
  end
  
  # [List-of PlayerInfo] [PID of DeclaredWinners] -> void
  defp initialize_game(players, game_results_pid) do
    starting_deck = Deck.create_deck()
    player_names = Enum.map(players, fn {:player, name, _} -> name end)
    {:ok, starting_hands, new_deck} = Deck.deal(starting_deck, player_names)

    # Elixir ranges are inclusive on both ends
    {starting_rows, _} = Enum.reduce(1..4, {[], new_deck}, fn _, {curr_rows, curr_deck} ->
      {:ok, drawn_card, new_deck} = Deck.draw_one(curr_deck)
      {[{:row, [drawn_card]} | curr_rows], new_deck}
    end)

    play_game(players, starting_hands, starting_rows, Map.new(player_names, fn n -> {n, 0} end), game_results_pid)
  end

  # [List-of PlayerInfo] [Hash-of PlayerID [List-of Card]] [List-of Row] Scores [PID of DeclaredWinners] -> void
  defp play_game(players, hands, rows, scores, game_results_pid) do
    Logging.log_rows(rows)
    start_round(1, players, hands, rows, scores, game_results_pid)
  end

  # Nat [List-of PlayerInfo] [Hash-of PlayerID [List-of Card]] [List-of Row] Scores [PID of DeclaredWinners] -> void
  defp start_round(round_no, players, hands, rows, scores, game_results_pid) do
    for {:player, name, pid} <- players do
      send pid, {:round, round_no, Map.get(hands, name), rows, self()}
    end
    # Send a timeout signal after one second
    Process.send_after self(), :timeout, 1000

    play_round([], round_no, players, hands, rows, scores, game_results_pid)
  end

  # [List-of Move] Nat [List-of PlayerInfo] [Hash-of PlayerID [List-of Card]] [List-of Row] Scores [PID of DeclaredWinners] -> void
  defp play_round(moves, round_no, players, hands, rows, scores, game_results_pid) do
    # Produce new hash containing entries with valid keys
    # [Hash-of A Any] [Set-of A] -> [Hash-of A Any]
    filter_keys = fn old_hash, valid_keys ->
      Enum.reduce(old_hash, %{}, fn {key, val}, new_hash ->
        if MapSet.member?(valid_keys, key) do
          Map.put(new_hash, key, val)
        else
          new_hash
        end
      end)
    end

    receive do
      :timeout ->
        valid_players = MapSet.new(Enum.map(moves, fn {:move, _, name, _} -> name end))
        new_hands = filter_keys.(hands, valid_players)
        new_scores = filter_keys.(scores, valid_players)

        conclude_round(
          moves,
          round_no,
          MapSet.to_list(valid_players),
          new_hands,
          rows,
          new_scores,
          game_results_pid
        )
      m = {:move, ^round_no, _name, _c} ->
        Logging.log_move(m)
        new_moves = [m | moves]
        if length(new_moves) == length(players) do
          conclude_round(new_moves, round_no, players, hands, rows, scores, game_results_pid)
        else
          play_round(new_moves, round_no, players, hands, rows, scores, game_results_pid)
        end
    end
  end

  # [List-of Move] Nat [List-of PlayerInfo] [Hash-of PlayerID [List-of Card]] [List-of Row] Scores [PID of DeclaredWinners] -> void
  defp conclude_round(moves, round_no, players, hands, rows, scores, game_results_pid) do
    {new_rows, new_scores} = Rules.play_round(rows, moves, scores)
    Logging.log_rows(new_rows)
    Logging.log_scores(new_scores)

    if round_no == 10 do
      winner_s = Rules.lowest_scores(new_scores)
      Logging.log_winners(winner_s)
      send game_results_pid, {:declared_winners, winner_s}
    else
      new_hands = Enum.reduce(moves, %{}, fn {:move, _no, name, c}, new_hands ->
        Map.put(new_hands, name, List.delete(Map.get(hands, name), c))
      end)

      start_round(round_no + 1, players, new_hands, new_rows, new_scores, game_results_pid)
    end
  end
end

defmodule Player do
  def spawn(client, dealer) do
    spawn fn ->
      name = register_player(client, dealer)
      loop(name, client, dealer)
    end
  end

  defp register_player(client, dealer) do
    {:ok, msg} = :gen_tcp.recv(client, 0)
    case Translator.parse(Poison.decode!(msg)) do
      player = {:player, name} ->
        send dealer, player
        name
    end
  end

  defp loop(name, client, dealer) do
    receive do
      {:round, round_no, cards, rows} ->
        move_request = {:move_request, round_no, cards, rows}
        :gen_tcp.send(client, Poison.encode!(Translator.unparse(move_request)))

        {:ok, msg} = :gen_tcp.recv(client, 0)
        case Translator.parse(Poison.decode!(msg)) do
          m = {:move, ^round_no, ^name, _c} ->
            send dealer, m
            loop(name, client, dealer)
        end
    end

  end
end

defmodule PlayerServer do
  def spawn(port, dealer) do
    spawn fn ->
      {:ok, socket} = :gen_tcp.listen(port, [:binary, packet: :line, active: false, reuseaddr: true])
      loop(socket, dealer)
    end
  end

  defp loop(socket, dealer) do
    {:ok, client} = :gen_tcp.accept(socket)
    pid = Player.spawn(client, dealer)
    :ok = :gen_tcp.controlling_process(client, pid)
    loop(socket, dealer)
  end
end

