class ForumController < ApplicationController
  def index
    @forums = Forum::list(session[:user_clan])
  end

  def show
    clan_id = session[:user_clan]
    forum_id = params[:id]
    @forum = Forum.read(clan_id, forum_id)
    @threads = ForumThread.list(clan_id, forum_id)
  end
end
