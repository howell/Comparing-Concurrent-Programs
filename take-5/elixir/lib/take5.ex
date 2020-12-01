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
    {starting_rows, _} = Enum.reduce(0..4, {[], new_deck}, fn _, {curr_rows, curr_deck} ->
      {:ok, drawn_card, new_deck} = Deck.draw_one(curr_deck)
      {[{:row, [drawn_card]} | curr_rows], new_deck}
    end)

    play_game(players, starting_hands, starting_rows, Map.new(player_names, fn n -> {n, 0} end), game_results_pid)
  end

  # [List-of PlayerInfo] [Hash-of PlayerID [List-of Card]] [List-of Row] Scores [PID of DeclaredWinners] -> void
  defp play_game(players, hands, rows, scores, game_results_pid) do
    Logging.log_rows(rows)
    play_round(1, players, hands, rows, scores, game_results_pid)
  end

  # Nat [List-of PlayerInfo] [Hash-of PlayerID [List-of Card]] [List-of Row] Scores [PID of DeclaredWinners] -> void
  defp play_round(round_no, players, hands, rows, scores, game_results_pid) do
    for {:player, name, pid} <- players do
      send pid, {:round, round_no, Map.get(hands, name), rows, self()}
    end

    moves = for _ <- 1..length(players) do
        receive do
          m ->
            Logging.log_move(m)
            m
        end
    end

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

      play_round(round_no + 1, players, new_hands, new_rows, new_scores, game_results_pid)
    end
  end
end

# Players that play the game
defmodule Player do
  # Spawn multiple players with the same card selection procedure
  # [List-of PlayerID] PlayModule -> [List-of PlayerInfo]
  def create_players(names, play_procedure) do
    for name <- names do
      create_player(name, play_procedure)
    end
  end

  # Spawn one player with the given card selection procedure
  # PlayerID PlayModule -> PlayerInfo
  def create_player(name, play_procedure) do
    pid = spawn fn -> play_round(name, play_procedure) end
    {:player, name, pid}
  end

  # Receive a request to play a card for a round and pick one to play
  # PlayerID PlayModule -> void
  defp play_round(name, play_procedure) do
    receive do
      {:round, round_no, cards, rows, pid} -> 
        selected_card = play_procedure.pick_card(rows, cards)
        Logging.log_player_decision(name, selected_card, cards)
        send pid, {:move, round_no, name, selected_card}
    end
    play_round(name, play_procedure)
  end
end

####### PlayModules #######
# a PlayModule describes how a player will choose a card in a round of the game.
#
# a PlayModule is a module that contains a function pick_card/2 with the signature:
# - [List-of Row] [List-of Card] -> Card

# a PlayModule that randomly selects a card to play
defmodule RandomPlay do
  # [List-of Row] [List-of Card] -> Card
  def pick_card(_rows, hand) do
    Enum.random(hand)
  end
end
