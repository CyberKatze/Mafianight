import type { Game, Player } from './types';
import axios from 'axios'
import { gameStore, gameAtom } from './store';

const apiUrl = import.meta.env.VITE_API_URL;

export async function fetchGame(gameId: string): Promise<Game> {
  const response = await axios.get(apiUrl + `/games/${gameId}`);
  return response.data;
}

export async function createGame(name: string, players: Player[]): Promise<Game> {
  const effect = (game: Game): void => { game.name = name; game.players = players };
  gameStore.set(gameAtom, effect);
  const token = localStorage.getItem('token');
  const response = await axios.post(apiUrl + '/games', gameStore.get(gameAtom), { headers: { Authorization: `Bearer ${token}` } });
  return response.data;
}

export async function updateGame(gameId: string, updatedGame: Game): Promise<Game> {
  const response = await axios.patch(`/api/games/${gameId}`, updatedGame);
  return response.data;
}


