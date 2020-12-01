# a Row is a {:row, [List-of Card]}
# a Move is a {:move, RoundNo, PlayerID, Card}

defmodule Rules do
  # Execute all submitted moves for a round
  # [List-of Row] [List-of Move] Scores -> {[List-of Row], Scores}
  def play_round(rows, moves, scores) do
    sorted_moves = Enum.sort_by(moves, fn {:move, _no, _player, {:card, rank, _}} -> rank end, &(&1 < &2))
    Enum.reduce(sorted_moves, {rows, scores}, fn {:move, _no, player, c}, {curr_rows, curr_scores} ->
      {new_rows, added_bulls} = play_card(c, curr_rows)
      {
        new_rows,
        Map.update!(curr_scores, player, &(&1 + added_bulls))
      }
    end)
  end

  # Fetch the players with the lowest scores
  # Scores -> [List-of PlayerID]
  def lowest_scores(scores) do
    lowest_score = Enum.min(Map.values(scores))
    Enum.filter(Map.keys(scores), fn player -> Map.get(scores, player) == lowest_score end)
  end

  # Determine the game state upon a card being played
  # Card [List-of Row] -> {[List-of Row], Nat}
  # Where the Nat represents the # of bulls picked up by the player
  defp play_card(c = {:card, played_rank, _}, rows) do
    selected_row = row_for_card(c, rows)
    {:row, row_cards} = selected_row
    [{:card, rank, _} | _] = row_cards

    if length(row_cards) == 5 or rank >= played_rank do
      {
        [{:row, [c]} | List.delete(rows, selected_row)],
        bulls_in_row(selected_row)
      }
    else
      {[{:row, [c | row_cards]} | List.delete(rows, selected_row)], 0}
    end
  end

  # Determine which row a card will be played on
  # Card [List-of Row] -> Row
  defp row_for_card({:card, played_rank, _}, rows) do
    playable_rows = Enum.filter(rows, fn {:row, [top | _rest]} -> 
      case top do
        {:card, comp_rank, _} -> comp_rank < played_rank
      end
    end)

    if length(playable_rows) == 0 do
      Enum.min_by(rows, fn r -> bulls_in_row(r) end)
    else
      Enum.max_by(playable_rows, fn {:row, [top | _rest]} ->
        case top do
          {:card, rank, _} -> rank
        end
      end)
    end
  end

  # Return the sum of bulls on cards in a row
  # Row -> Nat
  defp bulls_in_row({:row, cards}) do
    Enum.sum(Enum.map(cards, fn {:card, _, bulls} -> bulls end))
  end
end
