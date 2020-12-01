Dealer.spawn(Player.create_players([:a, :b, :c, :d, :e], RandomPlay), self())

receive do
  {:declared_winners, winners} -> IO.puts "Winners! #{inspect winners}"
end

