defmodule Logging do
  def log_rows(rows) do
    IO.puts "The current rows are:"
    for {:row, cards} <- rows do
      IO.puts inspect(Enum.reverse(cards))
    end
  end

  # TODO adding round number to move?
  def log_move({:move, name, card}) do
    IO.puts "Player #{name} plays card #{inspect card}"
  end

  def log_scores(scores) do
    for {player, score} <- scores do
      IO.puts "Player #{player} has #{score} bulls"
    end
  end

  def log_winners(winners) do
    IO.puts "The following players have the lowest score: #{inspect winners}"
  end

  def log_player_decision(name, card, hand) do
    IO.puts "Player #{name} selected card #{inspect card} from hand #{inspect hand}"
  end
end

