# a Row is a {:row, [List-of Card]}
# TEMP Scores is a [Hash-of PlayerID Number]
# a Move is a {:move, PlayerID, Card}

defmodule Rules do
  def play_round(rows, moves, scores) do
    sorted_moves = Enum.sort_by(moves, fn {:move, player, {:card, rank, _}} -> rank end, &(&1 < &2))
    Enum.reduce(sorted_moves, {rows, scores}, fn {:move, player, c}, {curr_rows, curr_scores} ->
      {new_rows, added_bulls} = play_card(c, curr_rows)
      {
        new_rows,
        Map.update!(curr_scores, player, &(&1 + added_bulls))
      }
    end)
  end

  defp play_card(c = {:card, played_rank, _}, rows) do
    selected_row = row_for_card(card, rows)
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

  defp row_for_card({:card, played_rank, _}, rows) do
    playable_rows = Enum.filter(rows, fn {:row, [top | rest]} -> 
      case top do
        {:card, comp_rank, _} -> comp_rank < played_rank
      end
    end)

    if length(playable_rows) == 0 do
      Enum.min_by(rows, fn r -> bulls_in_row(r) end)
    else
      Enum.max_by(playable_rows, fn {:row, [top | rest]} ->
        case top do
          {:card, rank, _} -> rank
        end
      end)
    end
  end

  defp bulls_in_row({:row, cards}) do
    Enum.sum(Enum.map(cards, fn {:card, _, bulls} -> bulls end))
  end
end
