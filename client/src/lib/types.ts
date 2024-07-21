export interface Role {
  name: string;
  desc: string;
  avatar: string;
  mafia: boolean;
}
export interface Player {
  name: string;
  role?: string;
  alive?: boolean;
}

export interface Event {
  subject: string;
  target: string;
  actionType: string;
}

export interface Game {
  id: string | undefined;
  name: string;
  players: Player[];
}

export interface PlayerWithRole {
  id: string;
  name: string;
  role: Role;
  alive?: boolean;
}
