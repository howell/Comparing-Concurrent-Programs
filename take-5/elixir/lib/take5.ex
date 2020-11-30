# Protocol
#
# a PlayerID is a Symbol
# a Hand is a [List-of Card]
# a Score is a Nat
# a Scores is a [Hash-of PlayerID Score]
#
# a Round is a {:round, [List-of Card], [List-of Row], PID}
# a PlayerInfo is a {:player, PlayerID, PID}
# a DeclaredWinner(s) is a {:declared_winner_s, [List-of PlayerID]}
#
# There are two roles in the protocol:
# 1. The Dealer, who manages a run of the game. There is one dealer per game instance.
# 2. The players, who attempt to win the game by communicating with the Dealer. There
#    are between 2 and 10 players per game.
#
# Conversations:
# There is a conversation about playing one round of the game. 
defmodule Dealer do
  def spawn(players, main_pid) do
    if length(players) < 2 or length(players) > 10 do
      raise "Take-5 is played with 2-10 players"
    else
      spawn fn -> initialize_game(players, main_pid) end
    end
  end
  
  defp initialize_game(players, main_pid) do
    starting_deck = Deck.create_deck()
    player_names = Enum.map(players, fn {:player, name, _} -> name end)
    {:ok, starting_hands, new_deck} = Deck.deal(starting_deck, player_names)
    # Elixir ranges are inclusive on both ends
    {starting_rows, _} = Enum.reduce(0..4, {[], new_deck}, fn _, {curr_rows, curr_deck} ->
      {:ok, drawn_card, new_deck} = Deck.draw_one(curr_deck)
      {[{:row, [drawn_card]} | curr_rows], new_deck}
    end)

    play_game(players, starting_hands, starting_rows, Map.new(player_names, fn n -> {n, 0} end), main_pid)
  end

  defp play_game(players, hands, rows, scores, main_pid) do
    play_round(0, players, hands, rows, scores, main_pid)
  end

  defp play_round(round_no, players, hands, rows, scores, main_pid) do
    for {:player, name, pid} <- players do
      send pid, {:round, Map.get(hands, name), rows, self()}
    end

    moves = for _ <- 1..10 do
        receive do
          m -> m
        end
    end

    {new_rows, new_scores} = Rules.play_round(rows, moves, scores)

    if round_no == 10 do
      winner_s = Rules.lowest_scores(scores)
      send main_pid, {:declared_winner_s, winner_s}
    else
      new_hands = Enum.reduce(moves, %{}, fn {:move, name, c}, new_hands ->
        Map.put(new_hands, name, List.delete(Map.get(hands, name), c))
      end)

      play_round(round_no + 1, players, new_hands, new_rows, new_scores, main_pid)
    end
  end
end

defmodule Player do
  def create_players(names, play_procedure) do
    for name <- names do
      create_player(name, play_procedure)
    end
  end

  def create_player(name, play_procedure) do
    pid = spawn fn -> play_round(name, play_procedure) end
    {:player, name, pid}
  end

  defp play_round(name, play_procedure) do
    receive do
      {:round, cards, rows, pid} -> 
        send pid, {:move, name, play_procedure.pick_card(rows, cards)}
    end
    play_round(name, play_procedure)
  end
end
# a Round is a {:round, [List-of Card], [List-of Row], PID}

defmodule RandomPlay do
  def pick_card(_rows, hand) do
    Enum.random(hand)
  end
end
