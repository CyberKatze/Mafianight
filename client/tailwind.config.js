/** @type {import('tailwindcss').Config} */
export default {
  content: [
    "./index.html",
    "./src/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      colors: {

          "darkgray": "#0E0D11",
          "violet" : "#7359F8",
          "lavendor": "#957DFD",
          "onyx" : "#1D1B22",
          "slate" : "#2A2733",
          "midnight": "#44475a",
          "stormy": "#6272a4",
          "mint": "#50fa7b",
          "blood": "#f98686",

         // Catppuccin Macchiato Palette
        rosewater: '#f5e0dc',
        flamingo: '#f2cdcd',
        pink: '#f5c2e7',
        mauve: '#cba6f7',
        red: '#f38ba8',
        maroon: '#eba0ac',
        peach: '#fab387',
        yellow: '#f9e2af',
        green: '#a6e3a1',
        teal: '#94e2d5',
        sky: '#89dceb',
        sapphire: '#74c7ec',
        blue: '#89b4fa',
        lavender: '#b4befe',
        text: {
          default: '#CAD3F5',
          inverted: '#24273A',
        },
        subtext: {
          default: '#B8C0E0',
          muted: '#A5ADCB',
        },
        overlay: {
          light: '#939AB7',
          default: '#8087A2',
          dark: '#6E738D',
        },
        surface: {
          light: '#585b70',
          default: '#45475a',
          dark: '#313244',
        },
        base: {
          light: '#24273A',
          dark: '#1e1e2e',
          default: '#1e1e2e',
        },
        mantle: '#181825',
        crust: '#11111b',

    },
  },
  plugins: [
      require('tailwind-scrollbar'),
  ],
}

}
