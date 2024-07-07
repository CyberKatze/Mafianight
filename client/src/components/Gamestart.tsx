import React from 'react';
import { useNavigate } from 'react-router-dom';

const GameStart: React.FC = () => {
  const navigate = useNavigate();

  const handleGameSetupRedirect = () => {
    navigate('/gamesetup');
  };

  return (
    <div className="flex flex-col items-center justify-start min-h-screen text-center">
      <div className="bg-[#1f1c2e] py-16 px-10 shadow-lg max-w-md mx-auto -mt-12 mb-8">
        <h1 className="text-2xl font-bold mb-6 text-white">Ready for a New Game?</h1>
        <p className="mb-6 text-white">
          To start a new game, click the button below and get ready for the fun!
        </p>
        <button
          onClick={handleGameSetupRedirect}
          className="bg-violet hover:bg-lavender text-white font-bold py-2 px-4 rounded shadow-md hover:shadow-lg transition duration-300"
        >
          Start New Game
        </button>
      </div>
    </div>
  );
};

export default GameStart;
