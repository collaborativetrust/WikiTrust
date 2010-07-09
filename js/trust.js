function showOrigin(revnum) {
  document.location.href = wgScriptPath + "/index.php?title=" + encodeURIComponent(wgPageName) + "&diff=" + encodeURIComponent(revnum);
}

function showOrg2(ev, revnum) {
  if(!ev.ctrlKey || !ev.altKey) return true;
  document.location.href = wgScriptPath + "/index.php?title=" + encodeURIComponent(wgPageName) + "&diff=" + encodeURIComponent(revnum) + "&trust";
  return false;
}

function ahref(ev) {
  if(ev.ctrlKey && ev.altKey) return false;
  return true;
}

// The Vote functionality
function voteCallback(http_request){
  if ((http_request.readyState == 4) && (http_request.status == 200)) {
    document.getElementById("vote-button-done").style.visibility = "visible";
    document.getElementById("vote-button").style.visibility = "hidden";
    //alert(http_request.responseText);
    var trustDiv = document.createElement('div');
    trustDriv.setAttribute('id', 'trust-div');
    var bodyContent = document.getElementById('bodyContent');
    var siteSub = document.getElementById('siteSub');
    var contentSub = document.getElementById('contentSub');
    var catlinks = document.getElementById('catlinks');
    bodyContent.innerHTML = '';
    bodyContent.appendChild(siteSub);
    bodyContent.appendChild(contentSub);
    bodyContent.appendChild(trustDiv);
    if (catlinks) bodyContent.appendChild(catlinks);
    trustDiv.innerHTML = http_request.responseText;
    return true;
  } else {
    // Turn off error reporting.
    //alert(http_request.responseText);
    return false;
  }
}

function getQueryVariable(variable) {
  var query = window.location.search.substring(1);
  var vars = query.split("&");
  for (var i=0;i<vars.length;i++) {
    var pair = vars[i].split("=");
    if (pair[0] == variable) {
      return pair[1];
    }
  } 
  return "";
}

function startVote(){
  var revID = getQueryVariable("oldid");
  if (revID == ""){
    revID = getQueryVariable("diff");
    if (revID == ""){
      revID = wgCurRevisionId;
    }
  }

  return sajax_do_call( "WikiTrust::ajax_recordVote", [wgUserName, wgArticleId, revID, wgPageName] , voteCallback ); 
}
