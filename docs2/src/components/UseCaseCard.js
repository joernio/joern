import React from 'react';
import Link from '@docusaurus/Link';
import { makeStyles } from '@material-ui/core/styles';
import Card from '@material-ui/core/Card';
import CardActions from '@material-ui/core/CardActions';
import CardActionArea from '@material-ui/core/CardActionArea';
import CardContent from '@material-ui/core/CardContent';
import Button from '@material-ui/core/Button';
import Typography from '@material-ui/core/Typography';

const useStyles = makeStyles({
  root: {
    minWidth: 200,
    minHeight: 80,
    borderRadius: 0,
    background: "#FEFEFE",
  },
  bullet: {
    display: 'inline-block',
    margin: '0 2px',
    transform: 'scale(0.8)',
  },
  title: {
    fontSize: 18,
  },
});

export default function UseCaseCard({ text, to }) {
  const classes = useStyles();
  const bull = <span className={classes.bullet}>•</span>;

  return (
    <Card className={classes.root} variant="outlined">
      <Link to={to}>
        <CardActionArea>
          <CardContent>
            <Typography component="h3">
              {text}
            </Typography>
          </CardContent>
        </CardActionArea>
      </Link>
    </Card>
  );
}
