# a Card is a {:card, Number, Number} where the first number is the value of the card and the second is the number of "bulls"
# A Deck is a [List-of Card]

defmodule Deck do
  # -> Deck
  def create_deck do
    deck = for i <- 1..104 do
      bulls =
        cond do
          rem(i, 55) == 0 -> 7
          rem(i, 11) == 0 -> 5
          rem(i, 10) == 0 -> 3
          rem(i, 5) == 0 -> 2
          true -> 1
        end
      {:card, i, bulls}
    end
    Enum.shuffle(deck)
  end

  # Deck -> (U {:ok, Card, Deck} {:error, String})
  def draw_one(deck) do
    if length(deck) == 0 do
      {:error, "Deck is empty"}
    else
      [first | rest] = deck
      {:ok, first, rest}
    end
  end

  # Deck Number -> (U {:ok, [List-of Card], Deck} {:error, String})
  def draw(deck, count) do
    if length(deck) < count do
      {:error, "Deck too small"}
    else
      {cards, new_deck} =
        Enum.reduce(1..count, {[], deck}, fn _, {cards, curr_deck} ->
          {:ok, new_card, new_deck} = Deck.draw_one(curr_deck)
          {[new_card | cards], new_deck}
        end)
      {:ok, cards, new_deck}
    end
  end

  # Deck [List-of PlayerID] -> (U {:ok, [Hash-of PlayerID Hand], Deck}, {:error, String})
  # ASSUME all PlayerIDs are unique
  def deal(deck, players) do
    if length(deck) < 10 * length(players) do
      {:error, "Too few cards to deal"}
    else
      {hands, new_deck} =
        Enum.reduce(players, {%{}, deck}, fn p, {hands, curr_deck} ->
          {:ok, cards, new_deck} = Deck.draw(curr_deck, 10)
          {Map.put(hands, p, cards), new_deck}
        end)
      {:ok, hands, new_deck}
    end
  end
end
