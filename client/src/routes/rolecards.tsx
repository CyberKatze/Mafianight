import React, { useEffect, useState } from 'react';
// import axios from 'axios';
import RoleCardComponent from '../components/RodeCard'

const mockRoleCards = [
  {
    id: 1,
    name: 'Doctor',
    avatar: 'https://img-cdn.pixlr.com/image-generator/history/65bb506dcb310754719cf81f/ede935de-1138-4f66-8ed7-44bd16efc709/medium.webp', 
    player: 'John',
    mafia: false,
  },
  {
    id: 2,
    name: 'God Father',
    avatar: 'https://img-cdn.pixlr.com/image-generator/history/65bb506dcb310754719cf81f/ede935de-1138-4f66-8ed7-44bd16efc709/medium.webp',
    player: 'Jane',
    mafia: true,
  },
  {
    id: 3,
    name: 'Folk',
    avatar: 'https://img-cdn.pixlr.com/image-generator/history/65bb506dcb310754719cf81f/ede935de-1138-4f66-8ed7-44bd16efc709/medium.webp',
    player: 'Jim',
    mafia: false,
  },
  {
    id: 4,
    name: 'Jocker',
    avatar: 'https://img-cdn.pixlr.com/image-generator/history/65bb506dcb310754719cf81f/ede935de-1138-4f66-8ed7-44bd16efc709/medium.webp',
    player: 'smith',
    mafia: true,
  },
];

export interface RoleCard {
  id: number;
  name: string;
  avatar: string;
  player: string;
  mafia: boolean;
}

const RoleCards: React.FC = () => {
  const [rolecards, setRoles] = useState<RoleCard[]>([]);
  const [currentIndex, setCurrentIndex] = useState(0);



  useEffect(() => {
    const fetchRoleCards = async () => {
      try {
        // const response = await axios.get(apiUrl + '/rolecards');
        setRoles(mockRoleCards);
      } catch (error) {
        console.error('Error fetching roles:', error);
      }
    };

    fetchRoleCards();
  }, []);


  const handleNext = () => {
    if (currentIndex < mockRoleCards.length - 1) {
      setCurrentIndex(currentIndex + 1);
    }
  };

  const handlePrevious = () => {
    if (currentIndex > 0) {
      setCurrentIndex(currentIndex - 1);
      console.log(currentIndex);
    }
  };


  return (
    <div>
      <div className="flex flex-wrap justify-center">
        <RoleCardComponent data={rolecards[currentIndex]} />
      </div>
      <div className="flex flex-wrap justify-center">
        {currentIndex > 0 &&
          <button type="button" 
                className=" bg-red text-white hover:bg-gray-700 focus:ring-4 focus:ring-gray-300 font-medium rounded-lg text-sm px-4 lg:px-5 py-2 lg:py-2.5 mr-2 dark:hover:bg-gray-700 focus:outline-none dark:focus:ring-gray-800"
                onClick={handlePrevious}>
            Previous
          </button>
        }
        {currentIndex < mockRoleCards.length-1 &&
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

export default RoleCards;
