import mafia from '../assets/images/mh3.svg';
import GameStart from '../components/Gamestart';

const Home = () => {
  return (
    <div className="homepage-container min-h-screen flex flex-col justify-start items-center">
      <div className="bg-violet svg-container w-full h-2/5 flex justify-start items-center">
        <img src={mafia} alt="Decorative SVG" className="decorative-svg w-auto mx-auto h-full object-contain object-center" />
      </div>
      <GameStart />
    </div>
  );
}

export default Home