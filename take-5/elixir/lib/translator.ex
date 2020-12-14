defmodule Translator do
  # Parse a DeclarePlayer message
  def parse(%{"player" => p}) do
    {:player, p}
  end

  # Parse a Move message
  # TODO change
  def parse(%{"player" => name, "round" => r, "play" => c}) do
    {:move, r, String.to_atom(name), parse_card(c)}
  end

  # Un-Parse a MoveRequest message
  def unparse({:move_request, n, cards, rows}) do
    %{"round" => n, "hand" => unparse_cards(cards), "rows" => Enum.map(rows, fn r -> unparse_row(r) end)}
  end

  # Un-Parse a GameOver message
  def unparse({:declared_winners, names}) do
    %{"winners" => Enum.map(names, fn n -> Atom.to_string(n) end)}
  end

  ## Parse Helpers

  defp parse_card(%{"rank" => r, "bulls" => b}) do
    {:card, r, b}
  end

  ## Un-Parse Helpers

  defp unparse_card({:card, r, b}) do
    %{"rank" => r, "bulls" => b}
  end

  defp unparse_cards(cs) do
    Enum.map(cs, fn c -> unparse_card(c) end)
  end

  defp unparse_row({:row, cards}) do
    %{"cards" => unparse_cards(cards)}
  end
end
