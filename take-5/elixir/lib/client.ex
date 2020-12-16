# The Protocol
# 
# Clients send a Player message upon connection
# Clients receive a Round message, and reply with a Move message

defmodule Client do
  def init(name, play_module) do
    {:ok, socket} = :gen_tcp.connect('localhost', 8900, [:binary, packet: :line, active: false, reuseaddr: true])
    Process.sleep(1000)
    register_player(socket, name)
    loop(socket, name, play_module)
  end

  defp register_player(socket, name) do
    IO.puts inspect(socket)
    :gen_tcp.send(socket, Poison.encode!(%{"player" => name}))
    IO.puts "banana"
  end

  defp loop(socket, name, play_module) do
    IO.puts "WOW"
    {:ok, msg} = :gen_tcp.recv(socket, 0)
    IO.puts "GASP"

    case Poison.decode!(msg) do
      %{"round" => r, "hand" => cards, "rows" => rows} ->
        selected_card = play_module.pick_card(rows, cards)
        :gen_tcp.send(socket, Poison.encode!(%{"player" => name, "round" => r, "play" => selected_card}))
        loop(socket, name, play_module)

      %{"winners" => names} ->
        if Enum.member?(names, name) do
          IO.puts "We won!"
        else
          IO.puts "We lost :("
        end
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
