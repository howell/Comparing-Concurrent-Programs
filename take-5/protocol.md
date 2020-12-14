# Protocol

## Data
A PlayerID is a string.

A Card is a:
```json
{
  "rank": "number",
  "bulls": "number"
}
```

A Row is a:
```json
{
  "cards": ["card"]
}
```

A MoveRequest is a:
```json
{
  "round": "number",
  "hand": ["card"],
  "rows": ["row"]
}
```

A Move is a:
```json
{
  "player": "string",
  "round": "number",
  "play": "card"
}
```

A DeclarePlayer is a:
```json
{
  "player": "string"
}
```

A GameOver is a:
```json
{
  "winners": ["PlayerID"]
}
```

## Messages
To participate in the game, clients must send a DeclarePlayer message to the server.

Clients receive a MoveRequest indicating the client must select a card for the next round of play.
The selected card is transmitted to the server in a Move message.

At the end of the game, all clients receive a GameOver message, indicating which players won.
