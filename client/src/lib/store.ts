import { atomWithImmer } from 'jotai-immer';
import { createStore } from 'jotai';
import type { Game, Role } from './types';

const gameAtom = atomWithImmer<Game>({ name: '', players: [] });
const gameStore = createStore();

gameStore.set(gameAtom, x => x)

const rolesAtom = atomWithImmer<Role[]>([]);

export { gameAtom, rolesAtom, gameStore }



