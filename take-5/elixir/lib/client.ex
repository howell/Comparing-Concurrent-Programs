defmodule Client do
  def init(name, play_module) do
    {:ok, socket} = :gen_tcp.connect('localhost', 8900, [active: false])
    register_player(socket, name)
    loop(socket, name, play_module)
  end

  defp register_player(socket, name) do
    :gen_tcp.send(socket, {:player, name})
  end

  defp loop(socket, name, play_module) do
    case :gen_tcp.recv(socket, 0) do
      # FIXME don't need the PID? maybe a MoveRequest struct should be introduced
      {:ok, {:round, round_no, cards, rows, pid}} ->
        selected_card = play_module.pick_card(rows, cards)
        :gen_tcp.send(socket, {:move, round_no, name, selected_card})
        loop(socket, name, play_module)
    end
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
