import React from 'react';

import Link from '@docusaurus/Link';

import { makeStyles } from '@material-ui/core/styles';
import Table from '@material-ui/core/Table';
import TableBody from '@material-ui/core/TableBody';
import TableCell from '@material-ui/core/TableCell';
import TableContainer from '@material-ui/core/TableContainer';
import TableHead from '@material-ui/core/TableHead';
import TableRow from '@material-ui/core/TableRow';

const useStyles = makeStyles({
  table: {
    minWidth: 550,
    color: '#ffffff',
  },
  cell: {
    color: 'white'
  }
});


export default function NodeStepsOverviewTable({ stepsInfo }) {
  const classes = useStyles();

  const rows = stepsInfo;

  return (
    <TableContainer>
      <Table className={classes.table} aria-label="Overview of Node-Type Steps">
        <TableHead>
          <TableRow>
            <TableCell className={classes.cell} style={{ width: 200 }}>Step</TableCell>
            <TableCell className={classes.cell} align="left">Description</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {stepsInfo.map((stepInfo) => (
            <TableRow key={stepInfo.kind}>
              <TableCell component="th" scope="row" >
                <Link to={"#" + stepInfo.queryComponent.toLowerCase()}>{stepInfo.queryComponent}</Link>
              </TableCell>
              <TableCell align="left" className={classes.cell} >{stepInfo.description}</TableCell>
            </TableRow>
          ))}
        </TableBody>
      </Table>
    </TableContainer>
  );
}
