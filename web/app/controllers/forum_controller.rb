class ForumController < ApplicationController
  def index
    @forums = Forum::list(session[:user_clan])
  end
end
