
var quicksearch = angular.module('quicksearch', ["angucomplete-ie8"]);

quicksearch.controller('QuicksearchCtrl', function QuicksearchCtrl($scope, $rootScope) {


  $scope.docinfo = [];
  $scope.selectedObject = function(selected) {
    if(selected && selected.originalObject.url) {
      window.location = selected.originalObject.url;
    } else {
      return "";
    }
  }
 $scope.addFilterSearch = function(filter) {
    $('#searchInput').val($('#searchInput').val()+" in:" + filter);
    console.log(filter);
  }

} );

// Helper function to access from outside angular scope

function initQuicksearchDocinfo(json) {
  var scope = angular.element($("#quicksearch")).scope();
  scope.$apply(function() {
    scope.docinfo = JSON.parse(json);
  });
  (function () {
    if($('.angucomplete-holder .input-group').offset().left==15){
      $('.dropdown-search').offset({left : $('.angucomplete-holder .input-group').offset().left});
    }else{
      $('.dropdown-search').css('left','');
    }
  })();
  $(window).resize(function(){
    if($('.angucomplete-holder .input-group').offset().left==15){
	  $('.dropdown-search').offset({left : $('.angucomplete-holder .input-group').offset().left});
    }else{
	  $('.dropdown-search').css('left','');
    }
  });
  $('#help-search').on('click', function (event) {
    $(this).parent().toggleClass('open');
  });
  $(document).on('click', function (e) {
	  if((!$('.group-search .dropdown-menu.dropdown-search').is(e.target))&&($('.group-search .dropdown-menu.dropdown-search').has(e.target).length === 0)&&($('.open').has(e.target).length === 0)){
      $('.group-search').removeClass('open');
    }
  });
};
