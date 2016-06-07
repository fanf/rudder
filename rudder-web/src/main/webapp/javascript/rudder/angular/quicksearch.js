var quicksearch = angular.module('quicksearch', ["ngTouch", "angucomplete-ie8"]);

quicksearch.controller('QuicksearchCtrl', ['$scope', '$http', '$rootScope', function QuicksearchCtrl($scope, $http, $rootScope) {

  alert("here we go");

  $scope.remoteUrlRequestFn = function(str) {
    return {q: str};
  };


} ] );


// Helper function to access from outside angular scope

function sometarget(target) {
  var scope = angular.element($("#GroupCtrl")).scope();
  scope.$apply(function() {
    scope.addExclude(target);
  });
};

