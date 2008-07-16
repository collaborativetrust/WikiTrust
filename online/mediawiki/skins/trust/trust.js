/**

Copyright (c) 2007
 Ian Pye   	     <ipye@soe.ucsc.edu>
 Jason Benterou      <jbentero@ucsc.edu>

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA
*/

/**
  The following are part of the trust in wikipedia project
  trust.cse.ucsc.edu

*/

var ctrlState = false;
function showOrigin(revnum) {
  //if (ctrlState) {
    document.location.href = "/index.php?title=" + wgPageName + "&oldid=" + revnum;
 // }
}

function checkForCtrlDown(event) {
  if (event.ctrlKey) { ctrlState = true; }
  //alert (event.keyCode + " " + event.ctrlKey + " " + ctrlState);
}

function clearCtrlState(event) {
  //alert(event.keyCode + " " + ctrlState);
  ctrlState = false;
}

function checkCtrlState(event) {
  //alert(ctrlState);
}
