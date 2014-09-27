require 'test_helper'

class ControlPanelControllerTest < ActionController::TestCase
  test "should get index" do
    get :index
    assert_response :success
  end

  test "should get newsfeed" do
    get :newsfeed
    assert_response :success
  end

  test "should get acl" do
    get :acl
    assert_response :success
  end

end
