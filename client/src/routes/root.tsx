import React from "react";
import Navbar from "../components/Navbar";
import { Outlet } from "react-router-dom";



const Root: React.FC = () => {
  return (
    <div className="min-h-screen flex flex-col">
      <Navbar />
  
      <div className="flex flex-1 bg-darkgray">
        <div id="detail" className="flex-grow">
          {/* Detail content goes here */}
          <Outlet />
        </div>
      </div>
    </div>
  );
}
export default Root;
