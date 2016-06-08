
var quicksearch = angular.module('quicksearch', ["ngTouch", "angucomplete-ie8"]);

quicksearch.controller('QuicksearchCtrl', function QuicksearchCtrl($scope) {


  $scope.test =  "test!!!";


  $scope.selectedObject = function(selected) {
    console.log("in selectedObject: " + selected.originalObject.name);
    if(selected && selected.originalObject.url) {
      eval(selected.originalObject.url);
    } else {
      //nothing
    }
  }


} );


// Helper function to access from outside angular scope
//
//function initQuicksearchUrl(url) {
//  var scope = angular.element($("#quicksearch")).scope();
//  scope.$apply(function() {
//    scope.setContextPath(url);
//  });
//};
//
