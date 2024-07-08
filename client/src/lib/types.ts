export interface Role {
  name: string;
  desc: string;
  avatar: string;
  mafia: boolean;
}
export interface Player {
  name: string;
  id: number;
  role: Role;
  alive?: boolean;
}

export interface Event{
  subject: string;
  target: string;
  actionType: string;
}