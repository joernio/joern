module.exports = {
  siteMetadata: {
    title: 'Joern',
    author: 'Chaitanya K Kamatham, Suchakra Sharma, Fabian Yamaguchi',
    description: 'Joern Website',
  },
  plugins: [
    'gatsby-plugin-react-helmet',
    {
      resolve: `gatsby-plugin-manifest`,
      options: {
        name: 'shiftleft-joern-website',
        short_name: 'website',
        start_url: '/',
        background_color: '#663399',
        theme_color: '#663399',
        display: 'minimal-ui',
        icon: 'src/assets/images/website-icon.png', // This path is relative to the root of the site.
      },
    },
    'gatsby-plugin-sass',
    'gatsby-plugin-offline',
    'gatsby-plugin-typescript',
    'gatsby-plugin-styled-components',
    {
	resolve: `gatsby-source-filesystem`,
	options: {
	    name: `mdx-pages`,
	    path: `${__dirname}/src/mdx`,
	},
    },
  ],
};
