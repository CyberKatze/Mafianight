import type { Game, PlayerWithRole, Role } from './types';
import axios from 'axios'
import { gameStore, gameAtom } from './store';

export const apiUrl = import.meta.env.VITE_API_URL;

export async function fetchGame(gameId: string): Promise<Game> {
  const response = await axios.get(apiUrl + `/games/${gameId}`);
  return response.data;
}
export async function fetchRoles(): Promise<Role[]> {
  const response = await axios.get(apiUrl + '/roles');
  return response.data;
}

export async function createGame(name: string, players: PlayerWithRole[]): Promise<Game> {
  const token = localStorage.getItem('token');
  const response = await axios.post(apiUrl + '/games', { name: name, players: players }, { headers: { Authorization: `Bearer ${token}` } });
  gameStore.set(gameAtom, response.data);
  return response.data;
}

export async function updateGame(gameId: string, updatedGame: Game): Promise<Game> {
  const response = await axios.patch(`/api/games/${gameId}`, updatedGame);
  return response.data;
}


