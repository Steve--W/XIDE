var Module=typeof pyodide._module!=="undefined"?pyodide._module:{};Module.checkABI(1);if(!Module.expectedDataFileDownloads){Module.expectedDataFileDownloads=0;Module.finishedDataFileDownloads=0}Module.expectedDataFileDownloads++;(function(){var loadPackage=function(metadata){var PACKAGE_PATH;if(typeof window==="object"){PACKAGE_PATH=window["encodeURIComponent"](window.location.pathname.toString().substring(0,window.location.pathname.toString().lastIndexOf("/"))+"/")}else if(typeof location!=="undefined"){PACKAGE_PATH=encodeURIComponent(location.pathname.toString().substring(0,location.pathname.toString().lastIndexOf("/"))+"/")}else{throw"using preloaded data can only be done on a web page or in a web worker"}var PACKAGE_NAME="more-itertools.data";var REMOTE_PACKAGE_BASE="more-itertools.data";if(typeof Module["locateFilePackage"]==="function"&&!Module["locateFile"]){Module["locateFile"]=Module["locateFilePackage"];err("warning: you defined Module.locateFilePackage, that has been renamed to Module.locateFile (using your locateFilePackage for now)")}var REMOTE_PACKAGE_NAME=Module["locateFile"]?Module["locateFile"](REMOTE_PACKAGE_BASE,""):REMOTE_PACKAGE_BASE;var REMOTE_PACKAGE_SIZE=metadata.remote_package_size;var PACKAGE_UUID=metadata.package_uuid;function fetchRemotePackage(packageName,packageSize,callback,errback){var xhr=new XMLHttpRequest;xhr.open("GET",packageName,true);xhr.responseType="arraybuffer";xhr.onprogress=function(event){var url=packageName;var size=packageSize;if(event.total)size=event.total;if(event.loaded){if(!xhr.addedTotal){xhr.addedTotal=true;if(!Module.dataFileDownloads)Module.dataFileDownloads={};Module.dataFileDownloads[url]={loaded:event.loaded,total:size}}else{Module.dataFileDownloads[url].loaded=event.loaded}var total=0;var loaded=0;var num=0;for(var download in Module.dataFileDownloads){var data=Module.dataFileDownloads[download];total+=data.total;loaded+=data.loaded;num++}total=Math.ceil(total*Module.expectedDataFileDownloads/num);if(Module["setStatus"])Module["setStatus"]("Downloading data... ("+loaded+"/"+total+")")}else if(!Module.dataFileDownloads){if(Module["setStatus"])Module["setStatus"]("Downloading data...")}};xhr.onerror=function(event){throw new Error("NetworkError for: "+packageName)};xhr.onload=function(event){if(xhr.status==200||xhr.status==304||xhr.status==206||xhr.status==0&&xhr.response){var packageData=xhr.response;callback(packageData)}else{throw new Error(xhr.statusText+" : "+xhr.responseURL)}};xhr.send(null)}function handleError(error){console.error("package error:",error)}var fetchedCallback=null;var fetched=Module["getPreloadedPackage"]?Module["getPreloadedPackage"](REMOTE_PACKAGE_NAME,REMOTE_PACKAGE_SIZE):null;if(!fetched)fetchRemotePackage(REMOTE_PACKAGE_NAME,REMOTE_PACKAGE_SIZE,function(data){if(fetchedCallback){fetchedCallback(data);fetchedCallback=null}else{fetched=data}},handleError);function runWithFS(){function assert(check,msg){if(!check)throw msg+(new Error).stack}Module["FS_createPath"]("/","lib",true,true);Module["FS_createPath"]("/lib","python3.7",true,true);Module["FS_createPath"]("/lib/python3.7","site-packages",true,true);Module["FS_createPath"]("/lib/python3.7/site-packages","more_itertools",true,true);Module["FS_createPath"]("/lib/python3.7/site-packages/more_itertools","tests",true,true);Module["FS_createPath"]("/lib/python3.7/site-packages","more_itertools-7.2.0-py3.7.egg-info",true,true);function DataRequest(start,end,audio){this.start=start;this.end=end;this.audio=audio}DataRequest.prototype={requests:{},open:function(mode,name){this.name=name;this.requests[name]=this;Module["addRunDependency"]("fp "+this.name)},send:function(){},onload:function(){var byteArray=this.byteArray.subarray(this.start,this.end);this.finish(byteArray)},finish:function(byteArray){var that=this;Module["FS_createPreloadedFile"](this.name,null,byteArray,true,true,function(){Module["removeRunDependency"]("fp "+that.name)},function(){if(that.audio){Module["removeRunDependency"]("fp "+that.name)}else{err("Preloading file "+that.name+" failed")}},false,true);this.requests[this.name]=null}};function processPackageData(arrayBuffer){Module.finishedDataFileDownloads++;assert(arrayBuffer,"Loading data file failed.");assert(arrayBuffer instanceof ArrayBuffer,"bad input to processPackageData");var byteArray=new Uint8Array(arrayBuffer);var curr;var compressedData={data:null,cachedOffset:124442,cachedIndexes:[-1,-1],cachedChunks:[null,null],offsets:[0,1256,2526,3760,4853,6124,7534,8969,10301,11530,12892,14263,15577,16766,18073,19356,20355,21652,22923,24068,25387,26680,27866,29274,30519,31829,33062,34233,35230,36475,37771,39086,40259,41593,42857,44078,45290,46637,47771,49061,50466,51681,52903,54160,55539,56828,57963,59060,60283,61395,62548,63405,64414,65566,66364,67255,68245,69272,70268,71250,72047,72862,73750,74630,75616,76477,77186,77883,78757,79692,80640,81520,82431,83141,84147,84917,85773,86830,87839,88711,89467,90443,91296,92252,93228,94201,95054,95831,96421,97241,98160,98966,99760,100412,101478,102452,103392,104368,105413,106296,107514,108653,109442,110596,110883,111201,111483,111804,112117,112424,112738,113033,113375,113699,114042,114365,115125,116295,117538,118687,120001,121104,122234,123455],sizes:[1256,1270,1234,1093,1271,1410,1435,1332,1229,1362,1371,1314,1189,1307,1283,999,1297,1271,1145,1319,1293,1186,1408,1245,1310,1233,1171,997,1245,1296,1315,1173,1334,1264,1221,1212,1347,1134,1290,1405,1215,1222,1257,1379,1289,1135,1097,1223,1112,1153,857,1009,1152,798,891,990,1027,996,982,797,815,888,880,986,861,709,697,874,935,948,880,911,710,1006,770,856,1057,1009,872,756,976,853,956,976,973,853,777,590,820,919,806,794,652,1066,974,940,976,1045,883,1218,1139,789,1154,287,318,282,321,313,307,314,295,342,324,343,323,760,1170,1243,1149,1314,1103,1130,1221,987],successes:[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]};compressedData.data=byteArray;assert(typeof Module.LZ4==="object","LZ4 not present - was your app build with  -s LZ4=1  ?");Module.LZ4.loadPackage({metadata:metadata,compressedData:compressedData});Module["removeRunDependency"]("datafile_more-itertools.data")}Module["addRunDependency"]("datafile_more-itertools.data");if(!Module.preloadResults)Module.preloadResults={};Module.preloadResults[PACKAGE_NAME]={fromCache:false};if(fetched){processPackageData(fetched);fetched=null}else{fetchedCallback=processPackageData}}if(Module["calledRun"]){runWithFS()}else{if(!Module["preRun"])Module["preRun"]=[];Module["preRun"].push(runWithFS)}};loadPackage({files:[{start:0,audio:0,end:87,filename:"/lib/python3.7/site-packages/more_itertools/__init__.py"},{start:87,audio:0,end:82946,filename:"/lib/python3.7/site-packages/more_itertools/more.py"},{start:82946,audio:0,end:98181,filename:"/lib/python3.7/site-packages/more_itertools/recipes.py"},{start:98181,audio:0,end:98181,filename:"/lib/python3.7/site-packages/more_itertools/tests/__init__.py"},{start:98181,audio:0,end:190543,filename:"/lib/python3.7/site-packages/more_itertools/tests/test_more.py"},{start:190543,audio:0,end:210021,filename:"/lib/python3.7/site-packages/more_itertools/tests/test_recipes.py"},{start:210021,audio:0,end:252870,filename:"/lib/python3.7/site-packages/more_itertools-7.2.0-py3.7.egg-info/PKG-INFO"},{start:252870,audio:0,end:253413,filename:"/lib/python3.7/site-packages/more_itertools-7.2.0-py3.7.egg-info/SOURCES.txt"},{start:253413,audio:0,end:253414,filename:"/lib/python3.7/site-packages/more_itertools-7.2.0-py3.7.egg-info/dependency_links.txt"},{start:253414,audio:0,end:253429,filename:"/lib/python3.7/site-packages/more_itertools-7.2.0-py3.7.egg-info/top_level.txt"}],remote_package_size:128538,package_uuid:"54958c37-a9bb-4889-904b-fd09d18e2814"})})();