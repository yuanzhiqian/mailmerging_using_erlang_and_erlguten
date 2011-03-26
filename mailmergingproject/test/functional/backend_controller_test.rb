require 'test_helper'

class BackendControllerTest < ActionController::TestCase
  test "should get saveXml" do
    get :saveXml
    assert_response :success
  end

end
