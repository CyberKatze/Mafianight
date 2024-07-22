import React, { useEffect, useState } from 'react';
// import axios from 'axios';
import VotingComponent from '../components/Voting'
import { players as mockPlayers } from "../lib/mockData"
import { PlayerWithRole } from '../lib/types';


export interface Player {
  id: number;
  name: string;
  mafia: boolean;
  avatar: string;
}

const Voting: React.FC = () => {
  const [players, setPlayers] = useState<PlayerWithRole[]>([]);
  const [currentIndex, setCurrentIndex] = useState(0);
  const [selectedIds, setSelectedIds] = useState<string[]>([]);

  useEffect(() => {
    const fetchPlayers = async () => {
      try {
        // const response = await axios.get(apiUrl + '/players');
        setPlayers(mockPlayers);
      } catch (error) {
        console.error('Error fetching roles:', error);
      }
    };

    fetchPlayers();
  }, []);

  const handleNext = () => {
    // console.log('selected: ' + selectedIds);
    if (selectedIds.length == 0) {
      return;
    }
    else if (currentIndex < mockPlayers.length - 1) {
      setCurrentIndex(currentIndex + 1);
    }
  };

  const handlePrevious = () => {
    if (currentIndex > 0) {
      setCurrentIndex(currentIndex - 1);
    }
  };

  // TODO: we should create the data for posting, I left this part as it is and will modify after API has been provided
  const handleSelectionChange = (isChecked: boolean, playerId: string, selectedId: string) => {
    const newSelectedIds = [...selectedIds];
    if (selectedId === null) {
      // just for using it
      console.log(playerId);
      return; // Handle potential null case (no selection)
    }
    else if (isChecked && !selectedIds.includes(selectedId)) {
      setSelectedIds([...newSelectedIds, selectedId]);
    }
    else if (!isChecked) {
      setSelectedIds(selectedIds.filter(id => id !== selectedId));
    }
  };

  return (
    <div>
      <div className="flex flex-wrap justify-center">
        <VotingComponent player={players[currentIndex]} players={players} onSelectionChange={handleSelectionChange} />
      </div>
      <div className="flex flex-wrap justify-center">
        {currentIndex > 0 &&
          <button type="button"
            className=" bg-red text-white hover:bg-gray-700 focus:ring-4 focus:ring-gray-300 font-medium rounded-lg text-sm px-4 lg:px-5 py-2 lg:py-2.5 mr-2 dark:hover:bg-gray-700 focus:outline-none dark:focus:ring-gray-800"
            onClick={handlePrevious}>
            Previous
          </button>
        }
        {currentIndex < mockPlayers.length - 1 &&
          <button type="button"
            className=" bg-violet text-white hover:bg-gray-700 focus:ring-4 focus:ring-gray-300 font-medium rounded-lg text-sm px-4 lg:px-5 py-2 lg:py-2.5 mr-2 dark:hover:bg-gray-700 focus:outline-none dark:focus:ring-gray-800"
            onClick={handleNext}>
            Next
          </button>
        }
      </div>
    </div>

  );
};

export default Voting;
