class ForumController < ApplicationController
  def index
    @forums = Forum::list(session[:user_clan])
  end

  def show
    @threads = ForumThread::list(session[:user_clan], params[:forum_id])
  end
end
