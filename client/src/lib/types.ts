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
  turn: Turn;
}
export interface Turn {
  phase: string;
  order: number;
}

export interface Game {
  id: string | undefined;
  name: string;
  players: PlayerWithRole[];
  currentTurn?: Turn | undefined;
}

export interface PlayerWithRole {
  id?: string;
  name: string;
  role: Role;
  alive?: boolean;
}
