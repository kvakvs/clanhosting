class ForumController < ApplicationController
  def index
    @forums = ForumModel::list(session[:user_clan])
  end

  def show
    clan_id = session[:user_clan]
    forum_id = params[:id]
    @forum_model = ForumModel.read(clan_id, forum_id)
    @threads = ThreadModel.list(clan_id, forum_id)
  end
end
