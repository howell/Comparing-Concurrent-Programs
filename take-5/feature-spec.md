Interface
---------
All users begin in the Authentication state. They can engage in two actions:
- Registering: create a user account and log the user into that account.
- Logging in: log a user into an existing account.

Once logged into an account, Users enter the Lobby.

From the Lobby, users can do the following:
- List rooms: list all existing rooms (games that have been created but not started) that a user can join
- Join a room: enter an existing room in the Lobby
- Create a room: Add a new entry to the list of rooms and become the host for that room (entering it)
- View results: List all results for completed games that the user has played in
- Log out: exit the lobby and return to the authentication state

There are two different roles that users may occupy when entering a room.

The first is the Host, who may do the following from a room:
- Start game: once there are between 2 and 10 players in the room, the Host can start the game and move all players from a room to a game.
- Exit room: return all users from the room to the Lobby and remove the room from the list of available rooms.

The second is the Guest, who can only perform the following action from a room:
- Leave room: return to the Lobby from the room

Once a game has started, all players (users in the room when the game was started by the host) may perform one of the following actions:
- Play a card upon request from the dealer of the game
- Leave the game, returning to the Lobby

When the game ends, all players are returned to the Lobby.
